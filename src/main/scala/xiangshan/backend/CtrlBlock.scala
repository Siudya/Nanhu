/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import xs.utils._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.mem.mdp.{LFST, SSIT, WaitTable}
import xiangshan.mem.LsqEnqIO
import xiangshan.mem._
import xiangshan.vector._
import xiangshan.vector.dispatch._
import xiangshan.vector.writeback._
import xiangshan.vector.VectorCtrlBlock
import xiangshan.backend.decode.{DecodeStage, FusionDecoder}
import xiangshan.backend.dispatch.{Dispatch, DispatchQueue, MemDispatch2Rs}
import xiangshan.backend.execute.fu.csr.PFEvent
import xiangshan.backend.rename.{Rename, RenameTableWrapper}
import xiangshan.backend.rob.{Rob, RobCSRIO, RobLsqIO, RobPtr}
import xiangshan.backend.issue.DqDispatchNode
import xiangshan.backend.execute.fu.csr.vcsr._
import xs.utils.perf.HasPerfLogging

class CtrlToFtqIO(implicit p: Parameters) extends XSBundle {
  val rob_commits = Vec(CommitWidth, Valid(new RobCommitInfo))
  val redirect = Valid(new Redirect)
}

class CtrlBlock(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val rob = LazyModule(new Rob)
  val wbMergeBuffer = LazyModule(new WbMergeBufferV2)
  val dispatchNode = new DqDispatchNode
  lazy val module = new CtrlBlockImp(this)
}

class CtrlBlockImp(outer: CtrlBlock)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasVectorParameters
  with HasCircularQueuePtrHelper
  with HasPerfEvents
  with HasPerfLogging
{
  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val cpu_halt = Output(Bool())
    val frontend = Flipped(new FrontendToCtrlIO)
    // to exu blocks
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    val vAllocPregs = Vec(VIRenameWidth, ValidIO(UInt(VIPhyRegIdxWidth.W)))
    val enqLsq = Flipped(new LsqEnqIO)
    val lqCancelCnt = Input(UInt(log2Up(LoadQueueSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
    val mmuEnable = Input(Bool())
    val sqDeq = Input(UInt(2.W))
    val lqDeq = Input(UInt(log2Up(CommitWidth + 1).W))
    // from int block
    val redirectIn = Input(Valid(new Redirect))
    val preWalk = Input(Valid(new Redirect))
    val memPredUpdate = Input(Valid(new MemPredUpdateReq))
    val stIn = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuInput)))
    val robio = new Bundle {
      // to int block
      val toCSR = new RobCSRIO
      val exception = ValidIO(new ExceptionInfo)
      // to mem block
      val lsq = new RobLsqIO
    }
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val perfInfo = Output(new Bundle{
      val ctrlInfo = new Bundle {
        val robFull   = Input(Bool())
        val intdqFull = Input(Bool())
        val fpdqFull  = Input(Bool())
        val lsdqFull  = Input(Bool())
      }
    })
    //to waitQueue
    val vstart = Input(UInt(7.W))
    val vcsrToRename  = Flipped(new VCSRWithVtypeRenameIO)
    //for debug
    val debug_int_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_vec_rat = Output(Vec(32, UInt(VIPhyRegIdxWidth.W)))
    val debug = new RobDbgIO

    val lsqVecDeqCnt = Input(new LsqVecDeqIO)
    val vecFaultOnlyFirst = Output(ValidIO(new ExuOutput))
  })
  require(outer.dispatchNode.out.count(_._2._1.isIntRs) == 1)
  require(outer.dispatchNode.out.count(_._2._1.isFpRs) == 1)
  require(outer.dispatchNode.out.count(_._2._1.isMemRs) == 1)
  private val intDispatch = outer.dispatchNode.out.filter(_._2._1.isIntRs).map(e => (e._1, e._2._1)).head
  private val fpDispatch  = outer.dispatchNode.out.filter(_._2._1.isFpRs).map(e => (e._1, e._2._1)).head
  private val lsDispatch  = outer.dispatchNode.out.filter(_._2._1.isMemRs).map(e => (e._1, e._2._1)).head
  private val intDeq  = intDispatch._1
  private val fpDeq   = fpDispatch._1
  private val lsDeq   = lsDispatch._1
  
  //vector
  private val vDispatch   = outer.dispatchNode.out.filter(_._2._1.isVecRs).map(e => (e._1, e._2._1)).head
  private val vpDispatch  = outer.dispatchNode.out.filter(_._2._1.isVpRs).map(e => (e._1, e._2._1)).head
  private val vDeq  = vDispatch._1
  private val vpDeq = vpDispatch._1
  private val vdWidth     = vDispatch._2.bankNum
  private val vpdWidth    = vpDispatch._2.bankNum
  private val mempdWidth  = coreParams.rsBankNum

  //Decode
  private val decode        = Module(new DecodeStage)
  private val fusionDecoder = Module(new FusionDecoder)

  //Dec-Rename Pipeline
  private val pipeHolds_dup = RegInit(VecInit(Seq.fill(DecodeWidth)(false.B)))
  private val decPipe = Module(new PipelineRouter(new CfCtrl, DecodeWidth, 2))
  decPipe.io.holds := pipeHolds_dup

  //Rename
  private val rename = Module(new Rename)
  private val rat = Module(new RenameTableWrapper)

  //memory
  private val ssit = Module(new SSIT)
  private val waittable = Module(new WaitTable)
  private val lfst = Module(new LFST)

  //Dispatch
  private val dispatch = Module(new Dispatch)
  private val memDispatch2Rs = Module(new MemDispatch2Rs)

  //DispatchQueue
  private val intDq = Module(new DispatchQueue(RenameWidth * 4, RenameWidth, intDispatch._2.bankNum))
  private val fpDq = Module(new DispatchQueue(RenameWidth * 4, RenameWidth, fpDispatch._2.bankNum))
  private val lsDq = Module(new DispatchQueue(RenameWidth * 4, RenameWidth, lsDispatch._2.bankNum))

  //ROB
  private val rob = outer.rob.module

  //Vector
  private val vCtrlBlock = Module(new VectorCtrlBlock(vdWidth, vpdWidth, mempdWidth))
  io.debug_vec_rat := vCtrlBlock.io.debug

  private val memDqArb = Module(new MemDispatchArbiter(coreParams.rsBankNum))
  private val wbMergeBuffer = outer.wbMergeBuffer.module
  vCtrlBlock.io.splitCtrl.allDone := RegNext(wbMergeBuffer.io.splitCtrl.allDone)
  vCtrlBlock.io.splitCtrl.allowNext := RegNext(wbMergeBuffer.io.splitCtrl.allowNext)

  wbMergeBuffer.io.vmbInit := vCtrlBlock.io.vmbInit
  io.vecFaultOnlyFirst.valid := RegNext(wbMergeBuffer.io.ffOut.valid, false.B)
  io.vecFaultOnlyFirst.bits := RegEnable(wbMergeBuffer.io.ffOut.bits, wbMergeBuffer.io.ffOut.valid)
  io.vecFaultOnlyFirst.bits.redirectValid := RegNext(wbMergeBuffer.io.ffOut.bits.redirectValid, false.B)
  io.vecFaultOnlyFirst.bits.redirect := RegEnable(wbMergeBuffer.io.ffOut.bits.redirect, wbMergeBuffer.io.ffOut.bits.redirectValid)

  //Redirect
  for (i <- 0 until CommitWidth) {
    val is_commit = rob.io.commits.commitValid(i) && rob.io.commits.isCommit
    io.frontend.toFtq.rob_commits(i).valid := RegNext(is_commit)
    io.frontend.toFtq.rob_commits(i).bits := RegEnable(rob.io.commits.info(i), is_commit)
  }
  private val redirectDelay = Pipe(io.redirectIn)
  io.frontend.toFtq.redirect := redirectDelay
  private val pendingRedirect = RegInit(false.B)
  when (redirectDelay.valid) {
    pendingRedirect := true.B
  }.elsewhen (RegNext(io.frontend.toFtq.redirect.valid, false.B)) {
    pendingRedirect := false.B
  }

  if (env.EnableTopDown) {
    val stage2Redirect_valid_when_pending = pendingRedirect && redirectDelay.valid

    val stage2_redirect_cycles = RegInit(false.B)                                         // frontend_bound->fetch_lantency->stage2_redirect
    val MissPredPending = RegInit(false.B); val branch_resteers_cycles = RegInit(false.B) // frontend_bound->fetch_lantency->stage2_redirect->branch_resteers
    val RobFlushPending = RegInit(false.B); val robFlush_bubble_cycles = RegInit(false.B) // frontend_bound->fetch_lantency->stage2_redirect->robflush_bubble
    val LdReplayPending = RegInit(false.B); val ldReplay_bubble_cycles = RegInit(false.B) // frontend_bound->fetch_lantency->stage2_redirect->ldReplay_bubble
    
    when(redirectDelay.valid && redirectDelay.bits.cfiUpdate.isMisPred) { MissPredPending := true.B }
    when(redirectDelay.valid && redirectDelay.bits.isFlushPipe)  { RobFlushPending := true.B }
    when(redirectDelay.valid && redirectDelay.bits.isLoadLoad)  { LdReplayPending := true.B }
    
    when (RegNext(io.frontend.toFtq.redirect.valid)) {
      when(pendingRedirect) {                             stage2_redirect_cycles := true.B }
      when(MissPredPending) { MissPredPending := false.B; branch_resteers_cycles := true.B }
      when(RobFlushPending) { RobFlushPending := false.B; robFlush_bubble_cycles := true.B }
      when(LdReplayPending) { LdReplayPending := false.B; ldReplay_bubble_cycles := true.B }
    }

    when(VecInit(decode.io.out.map(x => x.valid)).asUInt.orR){
      when(stage2_redirect_cycles) { stage2_redirect_cycles := false.B }
      when(branch_resteers_cycles) { branch_resteers_cycles := false.B }
      when(robFlush_bubble_cycles) { robFlush_bubble_cycles := false.B }
      when(ldReplay_bubble_cycles) { ldReplay_bubble_cycles := false.B }
    }

    XSPerfAccumulate("stage2_redirect_cycles", stage2_redirect_cycles)
    XSPerfAccumulate("branch_resteers_cycles", branch_resteers_cycles)
    XSPerfAccumulate("robFlush_bubble_cycles", robFlush_bubble_cycles)
    XSPerfAccumulate("ldReplay_bubble_cycles", ldReplay_bubble_cycles)
    XSPerfAccumulate("s2Redirect_pend_cycles", stage2Redirect_valid_when_pending)
  }

  //Decode
  decode.io.in      <> io.frontend.cfVec
  decode.io.intRat  <> rat.io.intReadPorts
  decode.io.fpRat   <> rat.io.fpReadPorts
  decode.io.csrCtrl := RegNext(io.csrCtrl)

  // memory dependency predict
  // when decode, send fold pc to mdp
  for (i <- 0 until DecodeWidth) {
    val mdp_foldpc = Mux(
      decode.io.out(i).fire,
      decode.io.in(i).bits.foldpc,
      rename.io.in(i).bits.cf.foldpc
    )
    ssit.io.raddr(i) := mdp_foldpc
    waittable.io.raddr(i) := mdp_foldpc
  }
  
  // currently, we only update mdp info when isReplay
  ssit.io.update := Pipe(io.memPredUpdate)
  ssit.io.csrCtrl := RegNext(io.csrCtrl)
  waittable.io.update := Pipe(io.memPredUpdate)
  waittable.io.csrCtrl := RegNext(io.csrCtrl)

  // LFST lookup and update
  lfst.io.redirect := redirectDelay
  lfst.io.storeIssue.zip(io.stIn).foreach({case(a, b) => a := Pipe(b)})
  lfst.io.csrCtrl <> RegNext(io.csrCtrl)
  lfst.io.dispatch <> dispatch.io.lfst

  //rename(only int and fp)
  //TODO: rob deq need to select
  val commitScalar  = Wire(new RobCommitIO)
  commitScalar := rob.io.commits
  for(((v, info), i) <- (commitScalar.commitValid zip commitScalar.info).zipWithIndex) {
    v := (!info.vecWen) && rob.io.commits.commitValid(i)
  }
  rat.io.robCommits     := commitScalar
  rat.io.intRenamePorts := rename.io.intRenamePorts
  rat.io.fpRenamePorts  := rename.io.fpRenamePorts

  //for debug
  io.debug_int_rat  := rat.io.debug_int_rat
  io.debug_fp_rat   := rat.io.debug_fp_rat

  decPipe.io.flush := redirectDelay.valid || pendingRedirect
  decPipe.io.in <> decode.io.out

  // pipeline between decode and rename
  for (i <- 0 until RenameWidth) {
    // fusion decoder
    val decodeHasException = io.frontend.cfVec(i).bits.exceptionVec(instrPageFault) || io.frontend.cfVec(i).bits.exceptionVec(instrAccessFault)
    val disableFusion = decode.io.csrCtrl.singlestep || !decode.io.csrCtrl.fusion_enable
    fusionDecoder.io.in(i).valid := io.frontend.cfVec(i).valid && !(decodeHasException || disableFusion)
    fusionDecoder.io.in(i).bits := io.frontend.cfVec(i).bits.instr
    if (i > 0) {
      fusionDecoder.io.inReady(i - 1) := decode.io.out(i).ready
    }

    // Pipeline
    val renamePipe = decPipe.io.out(0)(i)
    renamePipe.ready := rename.io.in(i).ready
    rename.io.in(i).valid := renamePipe.valid && !fusionDecoder.io.clear(i) && !rename.io.redirect.valid
    rename.io.in(i).bits  := renamePipe.bits
    rename.io.intReadPorts(i) := rat.io.intReadPorts(i).map(_.data)
    rename.io.fpReadPorts(i)  := rat.io.fpReadPorts(i).map(_.data)
    rename.io.waittable(i)    := RegEnable(waittable.io.rdata(i), decode.io.out(i).fire)
    rename.io.allowIn := decPipe.io.allowOut(0) && !rename.io.redirect.valid

    if (i < RenameWidth - 1) {
      // fusion decoder sees the raw decode info
      fusionDecoder.io.dec(i) := renamePipe.bits.ctrl
      rename.io.fusionInfo(i) := fusionDecoder.io.info(i)

      // update the first RenameWidth - 1 instructions
      decode.io.fusion(i) := fusionDecoder.io.out(i).valid && rename.io.out(i).fire
      when (fusionDecoder.io.out(i).valid) {
        fusionDecoder.io.out(i).bits.update(rename.io.in(i).bits.ctrl)
        // TODO: remove this dirty code for ftq update
        val sameFtqPtr = rename.io.in(i).bits.cf.ftqPtr.value === rename.io.in(i + 1).bits.cf.ftqPtr.value
        val ftqOffset0 = rename.io.in(i).bits.cf.ftqOffset
        val ftqOffset1 = rename.io.in(i + 1).bits.cf.ftqOffset
        val ftqOffsetDiff = ftqOffset1 - ftqOffset0
        val cond1 = sameFtqPtr && ftqOffsetDiff === 1.U
        val cond2 = sameFtqPtr && ftqOffsetDiff === 2.U
        val cond3 = !sameFtqPtr && ftqOffset1 === 0.U
        val cond4 = !sameFtqPtr && ftqOffset1 === 1.U
        rename.io.in(i).bits.ctrl.commitType := Mux(cond1, 4.U, Mux(cond2, 5.U, Mux(cond3, 6.U, 7.U)))
        XSError(!cond1 && !cond2 && !cond3 && !cond4, p"new condition $sameFtqPtr $ftqOffset0 $ftqOffset1\n")
      }
    }
  }

  rename.io.redirect    := Pipe(io.redirectIn)
  rename.io.robCommits  := commitScalar
  rename.io.ssit        := ssit.io.rdata
  rename.io.vcsrio    <> io.vcsrToRename
  rename.io.vlUpdate := Pipe(wbMergeBuffer.io.vlUpdate, 2)

  //pipeline between rename and dispatch
  private val flushRenamePipe = Wire(Bool())
  for (d <- 0 until RenameWidth) {
    for (i <- 0 until RenameWidth) {
      PipelineConnect(rename.io.out(i), dispatch.io.fromRename(d)(i), dispatch.io.recv(i), flushRenamePipe)
    }
  }

  //vector instr from scalar
  require(RenameWidth == VIDecodeWidth)
  vCtrlBlock.io.fromVtpRn := rename.io.toVCtl
  //TODO: vtype writeback here.
  vCtrlBlock.io.vtypewriteback := Pipe(io.vcsrToRename.vtypeWbToRename)

  vCtrlBlock.io.vmbAlloc <> wbMergeBuffer.io.allocate
  for((req, port) <- rob.io.enq.req.zip(vCtrlBlock.io.dispatchIn)) {
    port.bits := RegEnable(req.bits.robIdx, req.valid && rob.io.enq.canAccept)
    port.valid := RegNext(req.valid && rob.io.enq.canAccept && !rob.io.redirect.valid, false.B)
  }

  rob.io.wbFromMergeBuffer.zip(wbMergeBuffer.io.rob).foreach({case(a, b) => a := Pipe(b)})
  wbMergeBuffer.io.redirect := Pipe(io.redirectIn)
  
  
  val commitVector = Wire(new RobCommitIO)
  commitVector := rob.io.commits
  (commitVector.commitValid zip commitVector.walkValid).zip(commitVector.info).zipWithIndex.foreach {
    case (((cv, wv), info), i) => {
      cv := info.vecWen && rob.io.commits.commitValid(i)
      wv := info.vecWen && rob.io.commits.walkValid(i)
      wv := DontCare
    }
  }
  commitVector.isExtraWalk := rob.io.commits.isExtraWalk

  vCtrlBlock.io.commit := commitVector.Pipe
  vCtrlBlock.io.exception := Pipe(rob.io.exception)
  vCtrlBlock.io.redirect := io.redirectIn
  vCtrlBlock.io.vstart := io.vstart

  io.vAllocPregs := vCtrlBlock.io.vAllocPregs

  vCtrlBlock.io.in <> decPipe.io.out(1)
  vCtrlBlock.io.allowIn := decPipe.io.allowOut(1)
  
  vDeq  <> vCtrlBlock.io.vDispatch
  vpDeq <> vCtrlBlock.io.vpDispatch

  dispatch.io.hartId := io.hartId
  dispatch.io.redirect := redirectDelay
  //  dispatch.io.toIntDq <> intDq.io.enq
  intDq.io.enq.req := dispatch.io.toIntDq.req
  intDq.io.enq.needAlloc := dispatch.io.toIntDq.needAlloc
//  dispatch.io.toFpDq <> fpDq.io.enq
  fpDq.io.enq.req := dispatch.io.toFpDq.req
  fpDq.io.enq.needAlloc := dispatch.io.toFpDq.needAlloc
//  dispatch.io.toLsDq <> lsDq.io.enq
  lsDq.io.enq.req := dispatch.io.toLsDq.req
  lsDq.io.enq.needAlloc := dispatch.io.toLsDq.needAlloc
  for (i <- 1 until DecodeWidth) {
    dispatch.io.toIntDq.canAccept(i) := intDq.io.enq.canAccept_dup(i-1)
    dispatch.io.toFpDq.canAccept(i) := fpDq.io.enq.canAccept_dup(i-1)
    dispatch.io.toLsDq.canAccept(i) := lsDq.io.enq.canAccept_dup(i-1)
  }
  dispatch.io.toIntDq.canAccept(0) := intDq.io.enq.canAccept
  dispatch.io.toFpDq.canAccept(0) := fpDq.io.enq.canAccept
  dispatch.io.toLsDq.canAccept(0) := lsDq.io.enq.canAccept
  dispatch.io.allocPregs <> io.allocPregs
  dispatch.io.singleStep := RegNext(io.csrCtrl.singlestep)
  dispatch.io.enqRob <> rob.io.enq
  dispatch.io.vstart := RegNext(io.vstart)

  private val redirectDelay_dup_0 = Pipe(io.redirectIn)
  private val redirectDelay_dup_1 = Pipe(io.redirectIn)
  private val redirectDelay_dup_2 = Pipe(io.redirectIn)
  private val redirectDelay_dup_3 = Pipe(io.redirectIn)
  private val redirectDelay_dup_4 = Pipe(io.redirectIn)
  intDq.io.redirect := redirectDelay_dup_0
  fpDq.io.redirect := redirectDelay_dup_0
  lsDq.io.redirect := redirectDelay_dup_0
  intDq.io.redirect_dup := redirectDelay_dup_1
  fpDq.io.redirect_dup := redirectDelay_dup_2
  lsDq.io.redirect_dup := redirectDelay_dup_3

  intDeq <> intDq.io.deq
  fpDeq <> fpDq.io.deq

  //mem and vmem dispatch merge
  memDqArb.io.memIn <> lsDq.io.deq
  memDqArb.io.vmemIn <> vCtrlBlock.io.vmemDispatch
  memDqArb.io.redirect := redirectDelay

  memDispatch2Rs.io.redirect := redirectDelay_dup_4
  memDispatch2Rs.io.lcommit := io.lqDeq
  memDispatch2Rs.io.scommit := io.sqDeq
  memDispatch2Rs.io.lqCancelCnt := io.lqCancelCnt
  memDispatch2Rs.io.sqCancelCnt := io.sqCancelCnt
  memDispatch2Rs.io.enqLsq <> io.enqLsq
  memDispatch2Rs.io.in <> memDqArb.io.toMem2RS //lsDq.io.deq
  memDispatch2Rs.io.lsqVecDeqCnt <> io.lsqVecDeqCnt
  lsDeq <> memDispatch2Rs.io.out

  rob.io.hartId := io.hartId
  rob.io.mmuEnable := io.mmuEnable
  io.cpu_halt := DelayN(rob.io.cpu_halt, 5)
  private val robRedirect = Pipe(io.redirectIn)
  private val robPreWalk = Pipe(io.preWalk)
  rob.io.redirect.valid := robRedirect.valid | robPreWalk.valid
  rob.io.redirect.bits := Mux(robRedirect.valid, robRedirect.bits, robPreWalk.bits)
  when(io.redirectIn.valid) {
    pipeHolds_dup.foreach(_ := false.B)
  }.elsewhen(io.preWalk.valid) {
    pipeHolds_dup.foreach(_ := true.B)
  }
  flushRenamePipe := robRedirect.valid | robPreWalk.valid
  private val preWalkDbgValid = RegInit(false.B)
  private val preWalkDbgBits = RegEnable(io.preWalk.bits, io.preWalk.valid)
  when(robRedirect.valid) {
    preWalkDbgValid := false.B
  }.elsewhen(io.preWalk.valid) {
    preWalkDbgValid := true.B
  }
  when(preWalkDbgValid){
    assert(!io.preWalk.valid, "Continous 2 prewalk req is not expected!")
  }
  when(preWalkDbgValid & robRedirect.valid) {
    assert(preWalkDbgBits.robIdx >= robRedirect.bits.robIdx, "Prewalk req should pick younger inst!")
  }
  assert(PopCount(Cat(robRedirect.valid, robPreWalk.valid)) <= 1.U)

  // rob to int block
  io.robio.toCSR <> rob.io.csr
  // When wfi is disabled, it will not block ROB commit.
  rob.io.csr.wfiEvent := io.robio.toCSR.wfiEvent
  rob.io.wfi_enable := decode.io.csrCtrl.wfi_enable
  io.robio.toCSR.perfinfo.retiredInstr <> RegNext(rob.io.csr.perfinfo.retiredInstr)
  io.robio.exception := rob.io.exception

  // rob to mem block
  io.robio.lsq <> rob.io.lsq
  io.debug := rob.io.debug

  io.perfInfo.ctrlInfo.robFull := RegNext(rob.io.robFull)
  io.perfInfo.ctrlInfo.intdqFull := RegNext(intDq.io.dqFull)
  io.perfInfo.ctrlInfo.fpdqFull := RegNext(fpDq.io.dqFull)
  io.perfInfo.ctrlInfo.lsdqFull := RegNext(lsDq.io.dqFull)

  private val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := RegNext(io.csrCtrl.distribute_csr)
  private val csrevents = pfevent.io.hpmevent.slice(8,16)

  private val perfFromUnits = Seq(decode, rename, dispatch, intDq, fpDq, lsDq, rob).flatMap(_.getPerfEvents)
  private val perfFromIO    = Seq()
  private val perfBlock     = Seq()
  // let index = 0 be no event
  private val allPerfEvents = Seq(("noEvent", 0.U)) ++ perfFromUnits ++ perfFromIO ++ perfBlock

  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("CtrlBlock perfEvents Set", name, inc, i)
    }
  }

  private val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  generatePerfEvent()
}
