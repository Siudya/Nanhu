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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import xiangshan._
import xiangshan.backend.decode.{DecodeStage, FusionDecoder, ImmUnion}
import xiangshan.backend.dispatch.{Dispatch, DispatchQueue}
import xiangshan.backend.execute.fu.csr.PFEvent
import xiangshan.backend.rename.{Rename, RenameTableWrapper}
import xiangshan.backend.rob.{Rob, RobCSRIO, RobLsqIO}
import xiangshan.frontend.{FtqPtr, FtqRead, Ftq_RF_Components}
import xiangshan.mem.mdp.{LFST, SSIT, WaitTable}
import xiangshan.ExceptionNO._
import xiangshan.backend.issue.DqDispatchNode
import xiangshan.mem.{LsqEnqCtrl, LsqEnqIO}
import xs.utils._

class CtrlToFtqIO(implicit p: Parameters) extends XSBundle {
  val rob_commits = Vec(CommitWidth, Valid(new RobCommitInfo))
  val redirect = Valid(new Redirect)
}

class CtrlBlock(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val rob = LazyModule(new Rob)
  val dispatchNode = new DqDispatchNode

  lazy val module = new CtrlBlockImp(this)
}

class CtrlBlockImp(outer: CtrlBlock)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasCircularQueuePtrHelper
  with HasPerfEvents
{

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val cpu_halt = Output(Bool())
    val frontend = Flipped(new FrontendToCtrlIO)
    // to exu blocks
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    val enqLsq = Flipped(new LsqEnqIO)
    val lqCancelCnt = Input(UInt(log2Up(LoadQueueSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
    val sqDeq = Input(UInt(2.W))
    val ld_pc_read = Vec(exuParameters.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))
    // from int block
    val redirectIn = Input(Valid(new Redirect))
    val memPredUpdate = Input(new MemPredUpdateReq)
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
    val debug_int_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
  })
  require(outer.dispatchNode.out.count(_._2._1.isIntRs) == 1)
  require(outer.dispatchNode.out.count(_._2._1.isFpRs) == 1)
  require(outer.dispatchNode.out.count(_._2._1.isMemRs) == 1)
  private val intDispatch = outer.dispatchNode.out.filter(_._2._1.isIntRs).map(e => (e._1, e._2._1)).head
  private val fpDispatch = outer.dispatchNode.out.filter(_._2._1.isFpRs).map(e => (e._1, e._2._1)).head
  private val lsDispatch = outer.dispatchNode.out.filter(_._2._1.isMemRs).map(e => (e._1, e._2._1)).head
  private val intDeq = intDispatch._1
  private val fpDeq = fpDispatch._1
  private val lsDeq = lsDispatch._1

  private val decode = Module(new DecodeStage)
  private val fusionDecoder = Module(new FusionDecoder)
  private val rat = Module(new RenameTableWrapper)
  private val ssit = Module(new SSIT)
  private val waittable = Module(new WaitTable)
  private val rename = Module(new Rename)
  private val dispatch = Module(new Dispatch)
  private val intDq = Module(new DispatchQueue(dpParams.IntDqSize, RenameWidth, intDispatch._2.bankNum))
  private val fpDq = Module(new DispatchQueue(dpParams.FpDqSize, RenameWidth, fpDispatch._2.bankNum))
  private val lsDq = Module(new DispatchQueue(dpParams.LsDqSize, RenameWidth, lsDispatch._2.bankNum))
  private val rob = outer.rob.module

  for (i <- 0 until CommitWidth) {
    // why flushOut: instructions with flushPipe are not commited to frontend
    // If we commit them to frontend, it will cause flush after commit, which is not acceptable by frontend.
    val is_commit = rob.io.commits.commitValid(i) && rob.io.commits.isCommit && !rob.io.flushOut.valid
    io.frontend.toFtq.rob_commits(i).valid := RegNext(is_commit)
    io.frontend.toFtq.rob_commits(i).bits := RegEnable(rob.io.commits.info(i), is_commit)
  }
  io.frontend.toFtq.redirect := io.redirectIn

  val pendingRedirect = RegInit(false.B)
  when (io.redirectIn.valid) {
    pendingRedirect := true.B
  }.elsewhen (RegNext(io.frontend.toFtq.redirect.valid, false.B)) {
    pendingRedirect := false.B
  }

  if (env.EnableTopDown) {
    val stage2Redirect_valid_when_pending = pendingRedirect && io.redirectIn.valid

    val stage2_redirect_cycles = RegInit(false.B)                                         // frontend_bound->fetch_lantency->stage2_redirect
    val MissPredPending = RegInit(false.B); val branch_resteers_cycles = RegInit(false.B) // frontend_bound->fetch_lantency->stage2_redirect->branch_resteers
    val RobFlushPending = RegInit(false.B); val robFlush_bubble_cycles = RegInit(false.B) // frontend_bound->fetch_lantency->stage2_redirect->robflush_bubble
    val LdReplayPending = RegInit(false.B); val ldReplay_bubble_cycles = RegInit(false.B) // frontend_bound->fetch_lantency->stage2_redirect->ldReplay_bubble
    
    when(io.redirectIn.valid && io.redirectIn.bits.cfiUpdate.isMisPred) { MissPredPending := true.B }
    when(io.redirectIn.valid && io.redirectIn.bits.isCsr)  { RobFlushPending := true.B }
    when(io.redirectIn.valid && io.redirectIn.bits.isLoadLoad)  { LdReplayPending := true.B }
    
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

  decode.io.in <> io.frontend.cfVec
  decode.io.csrCtrl := RegNext(io.csrCtrl)
  decode.io.intRat <> rat.io.intReadPorts
  decode.io.fpRat <> rat.io.fpReadPorts

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
  ssit.io.update := RegEnable(io.memPredUpdate, io.memPredUpdate.valid)
  ssit.io.update.valid := RegNext(io.memPredUpdate.valid, false.B)
  ssit.io.csrCtrl := RegNext(io.csrCtrl)
  waittable.io.update := RegEnable(io.memPredUpdate, io.memPredUpdate.valid)
  waittable.io.update.valid := RegNext(io.memPredUpdate.valid, false.B)
  waittable.io.csrCtrl := RegNext(io.csrCtrl)

  // LFST lookup and update
  private val lfst = Module(new LFST)
  lfst.io.redirect := Pipe(io.redirectIn)
  lfst.io.storeIssue.zip(io.stIn).foreach({case(a, b) => a := Pipe(b)})
  lfst.io.csrCtrl <> RegNext(io.csrCtrl)
  lfst.io.dispatch <> dispatch.io.lfst

  rat.io.robCommits := rob.io.commits
  rat.io.intRenamePorts := rename.io.intRenamePorts
  rat.io.fpRenamePorts := rename.io.fpRenamePorts
  io.debug_int_rat := rat.io.debug_int_rat
  io.debug_fp_rat := rat.io.debug_fp_rat

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
    val renamePipe = PipelineNext(decode.io.out(i), rename.io.in(i).ready,
      io.redirectIn.valid || pendingRedirect)
    renamePipe.ready := rename.io.in(i).ready
    rename.io.in(i).valid := renamePipe.valid && !fusionDecoder.io.clear(i)
    rename.io.in(i).bits := renamePipe.bits
    rename.io.intReadPorts(i) := rat.io.intReadPorts(i).map(_.data)
    rename.io.fpReadPorts(i) := rat.io.fpReadPorts(i).map(_.data)
    rename.io.waittable(i) := RegEnable(waittable.io.rdata(i), decode.io.out(i).fire)

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

  rename.io.redirect := io.redirectIn
  rename.io.robCommits := rob.io.commits
  rename.io.ssit := ssit.io.rdata

  // pipeline between rename and dispatch
  for (i <- 0 until RenameWidth) {
    PipelineConnect(rename.io.out(i), dispatch.io.fromRename(i), dispatch.io.recv(i), io.redirectIn.valid)
  }

  dispatch.io.hartId := io.hartId
  dispatch.io.redirect := io.redirectIn
  dispatch.io.enqRob <> rob.io.enq
  dispatch.io.toIntDq <> intDq.io.enq
  dispatch.io.toFpDq <> fpDq.io.enq
  dispatch.io.toLsDq <> lsDq.io.enq
  dispatch.io.allocPregs <> io.allocPregs
  dispatch.io.singleStep := RegNext(io.csrCtrl.singlestep)

  intDq.io.redirect := Pipe(io.redirectIn)
  fpDq.io.redirect := Pipe(io.redirectIn)
  lsDq.io.redirect := Pipe(io.redirectIn)

  intDeq <> intDq.io.deq
  fpDeq <> fpDq.io.deq
  lsDeq <> lsDq.io.deq

  rob.io.hartId := io.hartId
  io.cpu_halt := DelayN(rob.io.cpu_halt, 5)
  rob.io.redirect := io.redirectIn
  outer.rob.generateWritebackIO(Some(outer), Some(this))

  // rob to int block
  io.robio.toCSR <> rob.io.csr
  // When wfi is disabled, it will not block ROB commit.
  rob.io.csr.wfiEvent := io.robio.toCSR.wfiEvent
  rob.io.wfi_enable := decode.io.csrCtrl.wfi_enable
  io.robio.toCSR.perfinfo.retiredInstr <> RegNext(rob.io.csr.perfinfo.retiredInstr)
  io.robio.exception := rob.io.exception
  io.robio.exception.bits.uop.cf.pc := flushPC

  // rob to mem block
  io.robio.lsq <> rob.io.lsq

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
  private val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  generatePerfEvent()
}
