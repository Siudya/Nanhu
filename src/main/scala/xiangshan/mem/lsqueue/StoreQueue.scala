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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xs.utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.MemoryOpConstants
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import difftest._
import difftest.common.DifftestMem
import freechips.rocketchip.util.SeqBoolBitwiseOps
import xiangshan.ExceptionNO.storeAccessFault
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.backend.issue.SelectPolicy
import xiangshan.mem.lsqueue.LSQExceptionGen
import xs.utils.perf.HasPerfLogging

class SqPtr(implicit p: Parameters) extends CircularQueuePtr[SqPtr](
  p => p(XSCoreParamsKey).StoreQueueSize
){
}

object SqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): SqPtr = {
    val ptr = Wire(new SqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class SqEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val lqCanAccept = Input(Bool())
  val needAlloc = Vec(exuParameters.LsExuCnt, Input(Bool()))
  val req = Vec(exuParameters.LsExuCnt, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(exuParameters.LsExuCnt, Output(new SqPtr))
  val reqNum = Input(UInt(exuParameters.LsExuCnt.W))
}

class DataBufferEntry (implicit p: Parameters)  extends DCacheBundle {
  val addr   = UInt(PAddrBits.W)
  val vaddr  = UInt(VAddrBits.W)
  val data   = UInt(DataBits.W)
  val mask   = UInt((DataBits/8).W)
  val wline = Bool()
  val sqPtr  = new SqPtr
  val active = Bool()
}

// Store Queue
class StoreQueue(implicit p: Parameters) extends XSModule with HasPerfLogging
  with HasDCacheParameters with HasCircularQueuePtrHelper with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val enq = new SqEnqIO
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // store addr, data is not included
    val storeInRe = Vec(StorePipelineWidth, Input(new LsPipelineBundle())) // store more mmio and exception
    val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new ExuOutput))) // store data, send to sq from rs
    val storeAddrIn = Vec(StorePipelineWidth, Flipped(Decoupled(new ExuOutput)))  // store addr
    val storeMaskIn = Vec(StorePipelineWidth, Flipped(Valid(new StoreMaskBundle))) // store mask, send to sq from rs
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReqWithVaddr)) // write committed store to sbuffer
    val mmioStout = DecoupledIO(new ExuOutput) // writeback uncached store
    val forward = Vec(LoadPipelineWidth, Flipped(new PipeLoadForwardQueryIO))
    val rob = Input(new RobPtr)
    val uncache = new UncacheWordIO
    // val refill = Flipped(Valid(new DCacheLineReq ))
    val exceptionAddr = new ExceptionAddrIO
    val sqempty = Output(Bool())
    val issuePtrExt = Output(new SqPtr) // used to wake up delayed load/store
    val sqFull = Output(Bool())
    val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
    val sqDeq = Output(UInt(2.W))
    val debug = new SqDbgIO
  })
  println("StoreQueue: size:" + StoreQueueSize)

  // data modules
//  val uop = Reg(Vec(StoreQueueSize, new MicroOp))
  val uop = RegInit(VecInit(List.fill(StoreQueueSize)(0.U.asTypeOf(new MicroOp))))
  // val data = Reg(Vec(StoreQueueSize, new LsqEntry))
  val dataModule = Module(new SQDataModule(
    numEntries = StoreQueueSize,
    numRead = StorePipelineWidth,
    numWbRead = StorePipelineWidth, //for stout
    numWrite = StorePipelineWidth,
    numForward = StorePipelineWidth
  ))
  dataModule.io := DontCare

  val v_pAddrModule = Module(new SQVPAddrModule(
    CommonNumRead = StorePipelineWidth,
    CommonNumWrite = StorePipelineWidth,
    numEntries = StoreQueueSize,
    NumForward = StorePipelineWidth,
    PAddrWidth = PAddrBits,
    PNumRead = 0,
    PNumWrite = 0,
    VAddrWidth = VAddrBits,
    VNumRead = 1,
    VNumWrite = 0,
    WbNumRead = StorePipelineWidth  //for stout
  ))
  v_pAddrModule.io := DontCare

  val dataBuffer = Module(new DatamoduleResultBuffer(new DataBufferEntry))
  val debug_paddr = Reg(Vec(StoreQueueSize, UInt((PAddrBits).W)))
  val debug_vaddr = Reg(Vec(StoreQueueSize, UInt((VAddrBits).W)))
  val debug_data = Reg(Vec(StoreQueueSize, UInt((XLEN).W)))

  // state & misc
  val allocated = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // sq entry has been allocated
  val readyToLeave = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // sq entry is to leaving
  val addrvalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // non-mmio addr is valid
  val datavalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // non-mmio data is valid
  val allvalid  = VecInit((0 until StoreQueueSize).map(i => addrvalid(i) && datavalid(i))) // non-mmio data & addr is valid
  val committed = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // inst has been committed by rob
  val active = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // inst is enabled
  val mmio = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // inst is an mmio store
//  val writebacked = RegInit(VecInit(List.fill(StoreQueueSize)(false.B)))  //inst has writebacked
  val writebacked_sta = RegInit(VecInit(List.fill(StoreQueueSize)(false.B)))  //inst has writebacked
  val writebacked_std = datavalid


  // ptr
  val enqPtrExt = RegInit(VecInit((0 until io.enq.req.length).map(_.U.asTypeOf(new SqPtr))))
  val rdataPtrExt = RegInit(VecInit((0 until StorePipelineWidth).map(_.U.asTypeOf(new SqPtr))))
  val deqPtrExt = RegInit(VecInit((0 until StorePipelineWidth).map(_.U.asTypeOf(new SqPtr))))
  val cmtPtrExt = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new SqPtr))))
  val issuePtrExt = RegInit(0.U.asTypeOf(new SqPtr))
  val validCounter = RegInit(0.U(log2Ceil(LoadQueueSize + 1).W))

  assert(cmtPtrExt.head <= enqPtrExt.head)
  assert(rdataPtrExt.head <= cmtPtrExt.head)
  assert(deqPtrExt.head <= rdataPtrExt.head)

  val enqPtr = enqPtrExt(0).value
  val deqPtr = deqPtrExt(0).value
  val cmtPtr = cmtPtrExt(0).value

  val validCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))
  val allowEnqueue = validCount <= (StoreQueueSize - 2).U

  val deqMask = UIntToMask(deqPtr, StoreQueueSize)
  val enqMask = UIntToMask(enqPtr, StoreQueueSize)
  val commitCount = Wire(UInt(log2Ceil(CommitWidth).W))

  // Read dataModule
  // rdataPtrExtNext and rdataPtrExtNext+1 entry will be read from dataModule
  val rdataPtrExtNext = WireInit(Mux(dataBuffer.io.enq(1).fire,
    VecInit(rdataPtrExt.map(_ + 2.U)),
    Mux(dataBuffer.io.enq(0).fire || io.mmioStout.fire,
      VecInit(rdataPtrExt.map(_ + 1.U)),
      rdataPtrExt
    )
  ))

  io.debug.enqPtr := enqPtrExt.head
  io.debug.deqPtr := deqPtrExt.head

  // deqPtrExtNext traces which inst is about to leave store queue
  //
  // io.sbuffer(i).fire is RegNexted, as sbuffer data write takes 2 cycles.
  // Before data write finish, sbuffer is unable to provide store to load
  // forward data. As an workaround, deqPtrExt and allocated flag update 
  // is delayed so that load can get the right data from store queue.
  //
  // Modify deqPtrExtNext and io.sqDeq with care!
  val deqPtrExtNext = Mux(RegNext(dataBuffer.io.deq(1).fire),
    VecInit(deqPtrExt.map(_ + 2.U)),
    Mux(RegNext(dataBuffer.io.deq(0).fire) || io.mmioStout.fire,
      VecInit(deqPtrExt.map(_ + 1.U)),
      deqPtrExt
    )
  )
  io.sqDeq := RegNext(Mux(RegNext(dataBuffer.io.deq(1).fire), 2.U,
    Mux(RegNext(dataBuffer.io.deq(0).fire) || io.mmioStout.fire, 1.U, 0.U)
  ))
  assert(!RegNext(RegNext(dataBuffer.io.deq(0).fire) && io.mmioStout.fire))

  for (i <- 0 until StorePipelineWidth) {
    dataModule.io.raddr(i) := rdataPtrExtNext(i).value
    v_pAddrModule.io.raddr(i) := rdataPtrExtNext(i).value

//    paddrModule.io.raddr(i) := rdataPtrExtNext(i).value
//    vaddrModule.io.raddr(i) := rdataPtrExtNext(i).value
  }

  // no inst will be committed 1 cycle before tval update
//  vaddrModule.io.raddr(StorePipelineWidth) := (cmtPtrExt(0) + commitCount).value
  v_pAddrModule.io.raddr_ext_v(0) := (cmtPtrExt(0) + commitCount).value

  /**
    * Enqueue at dispatch
    *
    * Currently, StoreQueue only allows enqueue when #emptyEntries > EnqWidth
    */
  io.enq.canAccept := allowEnqueue
  val canEnqueue = io.enq.req.map(_.valid)
  val enqCancel = io.enq.req.map(_.bits.robIdx.needFlush(io.brqRedirect))
  for (i <- 0 until io.enq.req.length) {
    val offset = if (i == 0) 0.U else PopCount(io.enq.needAlloc.take(i))
    val sqIdx = enqPtrExt(offset)
    val index = io.enq.req(i).bits.sqIdx.value
    when (canEnqueue(i) && !enqCancel(i)) {
      uop(index).robIdx := io.enq.req(i).bits.robIdx
      allocated(index) := true.B
      datavalid(index) := false.B
      addrvalid(index) := false.B
      committed(index) := false.B
      mmio(index) := false.B
      active(index) := true.B
      writebacked_sta(index) := false.B
      writebacked_std(index) := false.B
      readyToLeave(index) := false.B

      XSError(!io.enq.canAccept || !io.enq.lqCanAccept, s"must accept $i\n")
      XSError(index =/= sqIdx.value, s"must be the same entry $i\n")
    }
    io.enq.resp(i) := sqIdx
  }
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  /**
    * Update issuePtr when issue from rs
    */
  // update issuePtr
  val IssuePtrMoveStride = 4
  require(IssuePtrMoveStride >= 2)

  val issueLookupVec = (0 until IssuePtrMoveStride).map(issuePtrExt + _.U)
  val issueLookup = issueLookupVec.map(ptr => allocated(ptr.value) && addrvalid(ptr.value) && datavalid(ptr.value) && ptr =/= enqPtrExt(0))
  val nextIssuePtr = issuePtrExt + PriorityEncoder(VecInit(issueLookup.map(!_) :+ true.B))
  issuePtrExt := nextIssuePtr

  when (io.brqRedirect.valid) {
    issuePtrExt := Mux(
      isAfter(cmtPtrExt(0), deqPtrExt(0)),
      cmtPtrExt(0),
      deqPtrExtNext(0) // for mmio insts, deqPtr may be ahead of cmtPtr
    )
  }
  // send issuePtrExt to rs
  // io.issuePtrExt := cmtPtrExt(0)
  io.issuePtrExt := issuePtrExt

  //update STA exception info
  require(io.storeIn.length == 2)
  private val exceptionGen = Module(new LSQExceptionGen(StorePipelineWidth, FuConfigs.staCfg))
  exceptionGen.io.redirect := io.brqRedirect
  private val exceptionInfo = exceptionGen.io.out
  exceptionGen.io.in.zipWithIndex.foreach({case (d,i) =>
    val validCond = io.storeIn(i).valid && !io.storeIn(i).bits.uop.robIdx.needFlush(io.brqRedirect)
    d.bits.robIdx := RegEnable(io.storeIn(i).bits.uop.robIdx, validCond)
    d.bits.vaddr := RegEnable(io.storeIn(i).bits.vaddr, validCond)
    d.bits.segIdx := RegEnable(io.storeIn(i).bits.uop.segIdx, validCond)
    d.bits.sqIdx := RegEnable(io.storeIn(i).bits.uop.sqIdx, validCond)
    d.bits.lqIdx := DontCare
    d.bits.eVec := io.storeInRe(i).uop.cf.exceptionVec
    d.valid := RegNext(validCond & !io.storeIn(i).bits.miss, false.B)// miss will trigger replay, dont record excpt.
  })
  val mmioEvec = Wire(ExceptionVec())
  mmioEvec.foreach(_ := false.B)
  mmioEvec(storeAccessFault) := true.B
  exceptionGen.io.mmioUpdate.valid := io.uncache.resp.fire && io.uncache.resp.bits.error
  exceptionGen.io.mmioUpdate.bits.eVec := mmioEvec
  exceptionGen.io.mmioUpdate.bits.robIdx := io.rob
  exceptionGen.io.mmioUpdate.bits.vaddr := io.uncache.req.bits.addr
  exceptionGen.io.mmioUpdate.bits.segIdx := uop(deqPtr).segIdx
  exceptionGen.io.mmioUpdate.bits.sqIdx := deqPtrExt(0)
  exceptionGen.io.mmioUpdate.bits.lqIdx := DontCare

  exceptionGen.io.clean := false.B

  io.exceptionAddr.vaddr := exceptionInfo.bits.vaddr

  /**
    * Writeback store from store units
    *
    * Most store instructions writeback to regfile in the previous cycle.
    * However,
    *   (1) For an mmio instruction with exceptions, we need to mark it as addrvalid
    * (in this way it will trigger an exception when it reaches ROB's head)
    * instead of pending to avoid sending them to lower level.
    *   (2) For an mmio instruction without exceptions, we mark it as pending.
    * When the instruction reaches ROB's head, StoreQueue sends it to uncache channel.
    * Upon receiving the response, StoreQueue writes back the instruction
    * through arbiter with store units. It will later commit as normal.
    */

  // Write addr to sq
  for (i <- 0 until StorePipelineWidth) {
//    paddrModule.io.wen(i) := false.B
//    vaddrModule.io.wen(i) := false.B
    v_pAddrModule.io.wen(i) := false.B

    dataModule.io.mask.wen(i) := false.B
    val stWbIndex = io.storeIn(i).bits.uop.sqIdx.value
    when (io.storeIn(i).valid) {
      val addr_valid = !io.storeIn(i).bits.miss
      addrvalid(stWbIndex) := addr_valid //!io.storeIn(i).bits.mmio
      active(stWbIndex) := io.storeIn(i).bits.uop.loadStoreEnable
      v_pAddrModule.io.waddr(i) := stWbIndex
      v_pAddrModule.io.wdata_p (i) := io.storeIn(i).bits.paddr
      v_pAddrModule.io.wdata_v (i) := io.storeIn(i).bits.vaddr
      v_pAddrModule.io.wlineflag(i) := io.storeIn(i).bits.wlineflag
      v_pAddrModule.io.wen(i) := true.B
      debug_paddr(v_pAddrModule.io.waddr(i)) := v_pAddrModule.io.wdata_p(i)

      uop(stWbIndex).ctrl := io.storeIn(i).bits.uop.ctrl
      uop(stWbIndex).mergeIdx := io.storeIn(i).bits.uop.mergeIdx
      uop(stWbIndex).uopIdx := io.storeIn(i).bits.uop.uopIdx
      uop(stWbIndex).uopNum := io.storeIn(i).bits.uop.uopNum
      uop(stWbIndex).vctrl := io.storeIn(i).bits.uop.vctrl
      uop(stWbIndex).debugInfo := io.storeIn(i).bits.uop.debugInfo
      XSInfo("store addr write to sq idx %d pc 0x%x miss:%d vaddr %x paddr %x mmio %x\n",
        io.storeIn(i).bits.uop.sqIdx.value,
        io.storeIn(i).bits.uop.cf.pc,
        io.storeIn(i).bits.miss,
        io.storeIn(i).bits.vaddr,
        io.storeIn(i).bits.paddr,
        io.storeIn(i).bits.mmio
      )
    }

    // re-replinish mmio, for pma/pmp will get mmio one cycle later
    val storeInFireReg = RegNext(io.storeIn(i).fire && !io.storeIn(i).bits.miss)
    val stWbIndexReg = RegEnable(stWbIndex, io.storeIn(i).valid)
    when (storeInFireReg) {
      mmio(stWbIndexReg) := io.storeInRe(i).mmio
    }

    when(v_pAddrModule.io.wen(i)) {
      debug_vaddr(v_pAddrModule.io.waddr(i)) := v_pAddrModule.io.wdata_v(i)
    }

//    when(vaddrModule.io.wen(i)){
//      debug_vaddr(vaddrModule.io.waddr(i)) := vaddrModule.io.wdata(i)
//    }
  }

  // Write data to sq
  // Now store data pipeline is actually 2 stages
  for (i <- 0 until StorePipelineWidth) {
    dataModule.io.data.wen(i) := false.B
    val stWbIndex = io.storeDataIn(i).bits.uop.sqIdx.value
    // sq data write takes 2 cycles:
    // sq data write s0
    when (io.storeDataIn(i).fire) {
      // send data write req to data module
      dataModule.io.data.waddr(i) := stWbIndex
      dataModule.io.data.wdata(i) := Mux(io.storeDataIn(i).bits.uop.ctrl.fuOpType === LSUOpType.cbo_zero,
        0.U,
        genWdata(io.storeDataIn(i).bits.data, io.storeDataIn(i).bits.uop.ctrl.fuOpType(1,0))
      )
      dataModule.io.data.wen(i) := true.B

      debug_data(dataModule.io.data.waddr(i)) := dataModule.io.data.wdata(i)

      XSInfo("store data write to sq idx %d pc 0x%x data %x -> %x\n",
        io.storeDataIn(i).bits.uop.sqIdx.value,
        io.storeDataIn(i).bits.uop.cf.pc,
        io.storeDataIn(i).bits.data,
        dataModule.io.data.wdata(i)
      )
    }
    // sq data write s1
    when (
      RegNext(io.storeDataIn(i).fire)
      // && !RegNext(io.storeDataIn(i).bits.uop).robIdx.needFlush(io.brqRedirect)
    ) {
      datavalid(RegNext(stWbIndex)) := true.B
    }
  }

  // Write mask to sq
  for (i <- 0 until StorePipelineWidth) {
    // sq mask write s0
    when (io.storeMaskIn(i).fire) {
      // send data write req to data module
      dataModule.io.mask.waddr(i) := io.storeMaskIn(i).bits.sqIdx.value
      dataModule.io.mask.wdata(i) := io.storeMaskIn(i).bits.mask
      dataModule.io.mask.wen(i) := true.B
    }
  }

  /**
    * load forward query
    *
    * Check store queue for instructions that is older than the load.
    * The response will be valid at the next cycle after req.
    */
  // check over all lq entries and forward data from the first matched store
  for (i <- 0 until LoadPipelineWidth) {
    // Compare deqPtr (deqPtr) and forward.sqIdx, we have two cases:
    // (1) if they have the same flag, we need to check range(tail, sqIdx)
    // (2) if they have different flags, we need to check range(tail, LoadQueueSize) and range(0, sqIdx)
    // Forward1: Mux(same_flag, range(tail, sqIdx), range(tail, LoadQueueSize))
    // Forward2: Mux(same_flag, 0.U,                   range(0, sqIdx)    )
    // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise
    val differentFlag = deqPtrExt(0).flag =/= io.forward(i).sqIdx.flag
    val forwardMask = io.forward(i).sqIdxMask
    // all addrvalid terms need to be checked
    val addrValidVec = addrvalid.asUInt & allocated.asUInt & active.asUInt
    val dataValidVec = datavalid.asUInt
    val canForward1 = Mux(differentFlag, ~deqMask, deqMask ^ forwardMask).asUInt & addrValidVec
    val canForward2 = Mux(differentFlag, forwardMask, 0.U(StoreQueueSize.W)).asUInt & addrValidVec
    val needForward = Mux(differentFlag, (~deqMask).asUInt | forwardMask, deqMask ^ forwardMask)

    XSDebug(p"$i f1 ${Binary(canForward1)} f2 ${Binary(canForward2)} " +
      p"sqIdx ${io.forward(i).sqIdx} pa ${Hexadecimal(io.forward(i).paddr)}\n"
    )

    // do real fwd query (cam lookup in load_s1)
    dataModule.io.needForward(i)(0) := canForward1 & v_pAddrModule.io.forwardMmask_v(i).asUInt
    dataModule.io.needForward(i)(1) := canForward2 & v_pAddrModule.io.forwardMmask_v(i).asUInt

    v_pAddrModule.io.forwardMdata_v(i) := io.forward(i).vaddr
    v_pAddrModule.io.forwardMdata_p(i) := io.forward(i).paddr

    val vpmaskNotEqual = (
      (RegNext(v_pAddrModule.io.forwardMmask_p(i).asUInt) ^ RegNext(v_pAddrModule.io.forwardMmask_v(i).asUInt)) &
      RegNext(needForward) &
      RegNext(addrValidVec)
    ) =/= 0.U
    val vaddrMatchFailed = vpmaskNotEqual && RegNext(io.forward(i).valid)
    when (vaddrMatchFailed) {
      XSInfo("vaddrMatchFailed: pc %x pmask %x vmask %x\n",
        RegNext(io.forward(i).uop.cf.pc),
        RegNext(needForward & v_pAddrModule.io.forwardMmask_p(i).asUInt),
        RegNext(needForward & v_pAddrModule.io.forwardMmask_v(i).asUInt)
      )
    }
    XSPerfAccumulate("vaddr_match_failed", vpmaskNotEqual)
    XSPerfAccumulate("vaddr_match_really_failed", vaddrMatchFailed)

    // Fast forward mask will be generated immediately (load_s1)
    io.forward(i).forwardMaskFast := dataModule.io.forwardMaskFast(i)

    // Forward result will be generated 1 cycle later (load_s2)
    io.forward(i).forwardMask := dataModule.io.forwardMask(i)
    io.forward(i).forwardData := dataModule.io.forwardData(i)

    // If addr match, data not ready, mark it as dataInvalid
    // load_s1: generate dataInvalid in load_s1 to set fastUop
    val sqForwardMaskFast = dataModule.io.forwardMaskFast(i).asUInt
    val loadMask = io.forward(i).mask
    val dataInvalidMask = addrValidVec.asUInt & (~dataValidVec).asUInt & v_pAddrModule.io.forwardMmask_v(i).asUInt & needForward
    io.forward(i).dataInvalidFast := dataInvalidMask.orR && (sqForwardMaskFast & loadMask).orR
    val dataInvalidMaskReg = RegEnable(dataInvalidMask, io.forward(i).valid)
    // load_s2
    io.forward(i).dataInvalid := RegEnable(io.forward(i).dataInvalidFast, false.B, io.forward(i).valid)
    // check if vaddr forward mismatched
    io.forward(i).matchInvalid := vaddrMatchFailed
    val dataInvalidMaskRegWire = Wire(UInt(StoreQueueSize.W))
    dataInvalidMaskRegWire := dataInvalidMaskReg // make chisel happy
    io.forward(i).dataInvalidSqIdx := PriorityEncoder(dataInvalidMaskRegWire)
  }

  /**
    * Memory mapped IO / other uncached operations
    *
    * States:
    * (1) writeback from store units: mark as pending
    * (2) when they reach ROB's head, they can be sent to uncache channel
    * (3) response from uncache channel: mark as datavalidmask.wen
    * (4) writeback to ROB (and other units): mark as writebacked
    * (5) ROB commits the instruction: same as normal instructions
    */
  //(2) when they reach ROB's head, order store will be processed.
  private val deqUop = uop(deqPtr)
  private val deqMmio = mmio(deqPtr)
  private val s_idle :: s_req_mmio :: s_resp_mmio :: s_wb_mmio :: s_end :: Nil = Enum(5)
  private val mmio_state = RegInit(s_idle)
  switch(mmio_state) {
    is(s_idle) {
      when(RegNext(readyToLeave(deqPtr) && deqMmio && allocated(deqPtr) && allvalid(deqPtr))) {
        mmio_state := s_req_mmio
      }
    }
    is(s_req_mmio) {
      when(io.uncache.req.fire) {
        mmio_state := s_resp_mmio
      }
    }
    is(s_resp_mmio) {
      when(io.uncache.resp.fire){
        mmio_state := s_wb_mmio
      }
    }
    is(s_wb_mmio) {
      when (io.mmioStout.fire) {
        mmio_state := s_end
      }
    }
    is(s_end){
      mmio_state := s_idle
    }
  }

  io.uncache.req.valid := mmio_state === s_req_mmio
  io.uncache.req.bits.robIdx := DontCare
  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XWR
//  io.uncache.req.bits.addr := paddrModule.io.rdata(0) // data(deqPtr) -> rdata(0)
  io.uncache.req.bits.addr := v_pAddrModule.io.rdata_p(0) // data(deqPtr) -> rdata(0)
  io.uncache.req.bits.data := dataModule.io.rdata(0).data
  io.uncache.req.bits.mask := dataModule.io.rdata(0).mask

  // CBO op type check can be delayed for 1 cycle,
  // as uncache op will not start in s_idle
  val cbo_mmio_addr = Cat(v_pAddrModule.io.rdata_p(0)(PAddrBits - 1, 2),0.U(2.W)) // clear lowest 2 bits for op
//  val cbo_mmio_addr = paddrModule.io.rdata(0) >> 2 << 2 // clear lowest 2 bits for op
  val cbo_mmio_op = 0.U //TODO
  val cbo_mmio_data = cbo_mmio_addr | cbo_mmio_op
  when(RegNext(LSUOpType.isCbo(uop(deqPtr).ctrl.fuOpType))){
    io.uncache.req.bits.addr := DontCare // TODO
//    io.uncache.req.bits.data := paddrModule.io.rdata(0)
    io.uncache.req.bits.data := v_pAddrModule.io.rdata_p(0)
    io.uncache.req.bits.mask := DontCare // TODO
  }

  io.uncache.req.bits.id   := DontCare
  io.uncache.req.bits.instrtype   := DontCare

  when(io.uncache.req.fire){
    XSDebug(
      p"uncache req: pc ${Hexadecimal(uop(deqPtr).cf.pc)} " +
      p"addr ${Hexadecimal(io.uncache.req.bits.addr)} " +
      p"data ${Hexadecimal(io.uncache.req.bits.data)} " +
      p"op ${Hexadecimal(io.uncache.req.bits.cmd)} " +
      p"mask ${Hexadecimal(io.uncache.req.bits.mask)}\n"
    )
  }

  // (3) response from uncache channel: mark as datavalid
  io.uncache.resp.ready := true.B
  io.mmioStout := DontCare
  // (4) writeback to ROB (and other units): mark as writebacked
  val defaultEVec = Wire(ExceptionVec())
  defaultEVec.foreach(_ := false.B)
  val excptHit = exceptionInfo.valid && deqUop.robIdx === exceptionInfo.bits.robIdx && deqUop.segIdx === exceptionInfo.bits.segIdx
  io.mmioStout.valid := (mmio_state === s_wb_mmio)
  io.mmioStout.bits.uop := deqUop
  io.mmioStout.bits.uop.cf.exceptionVec := Mux(excptHit, exceptionInfo.bits.eVec, defaultEVec)
  io.mmioStout.bits.uop.sqIdx := deqPtrExt(0)
  io.mmioStout.bits.data := dataModule.io.rdata(0).data // dataModule.io.rdata.read(deqPtr)
  io.mmioStout.bits.redirectValid := false.B
  io.mmioStout.bits.redirect := DontCare
  io.mmioStout.bits.debug.isMMIO := true.B
  io.mmioStout.bits.debug.paddr := DontCare
  io.mmioStout.bits.debug.isPerfCnt := false.B
  io.mmioStout.bits.fflags := DontCare
  io.mmioStout.bits.debug.vaddr := DontCare
  when(io.mmioStout.fire){
    allocated(deqPtr) := false.B
  }

  /**
    * ROB commits store instructions (mark them as committed)
    *
    * (1) When store commits, mark it as committed.
    * (2) They will not be cancelled and can be sent to lower level.
    */
  private val readyToDeq = Reg(Vec(StoreQueueSize, Bool()))
  for (i <- 0 until StoreQueueSize) {
    readyToDeq(i) := readyToLeave(i) & writebacked_sta(i) & writebacked_std(i) & allocated(i) &
      !(uop(i).robIdx === exceptionInfo.bits.robIdx && exceptionInfo.valid)
  }
  private val cmtVec = Seq.tabulate(CommitWidth)({idx =>
    val ptr = cmtPtrExt(idx)
    readyToDeq(ptr.value) & ptr < enqPtrExt.head & !io.brqRedirect.valid
  })
  private val cmtBlocked = cmtVec.map(!_) :+ true.B
  commitCount := PriorityEncoder(cmtBlocked)
  uop.zip(readyToLeave).zipWithIndex.foreach({ case ((u, r), idx) =>
    val updateReadyToLeave = Wire(Bool())
    updateReadyToLeave := u.robIdx <= io.rob && allocated(idx) && !r
    when(updateReadyToLeave) {
      r := true.B
    }
  })

  for (i <- 0 until CommitWidth) {
    when (commitCount > i.U) {
      committed(cmtPtrExt(i).value) := true.B
    }
  }
  when(io.mmioStout.fire){
    cmtPtrExt := cmtPtrExt.map(_ + 1.U)
  }.elsewhen(commitCount.orR){
    cmtPtrExt := cmtPtrExt.map(_ + commitCount)
  }


  // committed stores will not be cancelled and can be sent to lower level.
  // remove retired insts from sq, add retired store to sbuffer

  // Read data from data module
  // As store queue grows larger and larger, time needed to read data from data
  // module keeps growing higher. Now we give data read a whole cycle.

  // For now, data read logic width is hardcoded to 2
  require(StorePipelineWidth == 2) // TODO: add EnsbufferWidth parameter
  val mmioStall = mmio(rdataPtrExt(0).value)
  for (i <- 0 until StorePipelineWidth) {
    val ptr = rdataPtrExt(i).value
    dataBuffer.io.enq(i).valid := allocated(ptr) && committed(ptr) && !mmioStall
    // Note that store data/addr should both be valid after store's commit
    assert(!dataBuffer.io.enq(i).valid || allvalid(ptr))
    assert(!(dataBuffer.io.enq(i).valid && mmio(rdataPtrExt(i).value)))
    dataBuffer.io.enq(i).bits.addr  := v_pAddrModule.io.rdata_p(i)
    dataBuffer.io.enq(i).bits.vaddr := v_pAddrModule.io.rdata_v(i)
    dataBuffer.io.enq(i).bits.data  := dataModule.io.rdata(i).data
    dataBuffer.io.enq(i).bits.mask  := dataModule.io.rdata(i).mask
    dataBuffer.io.enq(i).bits.wline := v_pAddrModule.io.rlineflag_v_p(i)
    dataBuffer.io.enq(i).bits.sqPtr := rdataPtrExt(i)
    dataBuffer.io.enq(i).bits.active := active(ptr)
  }


  def getEvenBits(input: UInt): UInt = {
    VecInit((0 until StoreQueueSize / 2).map(i => {
      input(2 * i)
    })).asUInt
  }

  def getOddBits(input: UInt): UInt = {
    VecInit((0 until StoreQueueSize / 2).map(i => {
      input(2 * i + 1)
    })).asUInt
  }

  def getFirstOne(mask: Vec[Bool], startMask: UInt) = {
    val length = mask.length
    val highBits = (0 until length).map(i => mask(i) & ~startMask(i))
    val highBitsUint = Cat(highBits.reverse)
    PriorityEncoder(Mux(highBitsUint.orR, highBitsUint, mask.asUInt))
  }

  def toVec(a: UInt): Vec[Bool] = {
    VecInit(a.asBools)
  }

    (0 until StorePipelineWidth).foreach({ case i => {
      io.storeAddrIn(i).ready := true.B
      when(io.storeAddrIn(i).fire){
        writebacked_sta(io.storeAddrIn(i).bits.uop.sqIdx.value) := true.B
      }
    }})


  // Send data stored in sbufferReqBitsReg to sbuffer
  private val deqHeadActive = dataBuffer.io.deq.head.valid && dataBuffer.io.deq.head.bits.active
  for (i <- 0 until StorePipelineWidth) {
    val thisActive = dataBuffer.io.deq(i).bits.active
    if(i == 0){
      io.sbuffer(i).valid := dataBuffer.io.deq(i).valid && thisActive
      dataBuffer.io.deq(i).ready := io.sbuffer(i).ready || !thisActive
    } else {
      io.sbuffer(i).valid := dataBuffer.io.deq(i).valid && thisActive && deqHeadActive
      dataBuffer.io.deq(i).ready := io.sbuffer(i).ready && thisActive && deqHeadActive
    }
    // Write line request should have all 1 mask
    assert(!(io.sbuffer(i).valid && io.sbuffer(i).bits.wline && !io.sbuffer(i).bits.mask.andR))
    io.sbuffer(i).bits.cmd   := MemoryOpConstants.M_XWR
    io.sbuffer(i).bits.addr  := dataBuffer.io.deq(i).bits.addr
    io.sbuffer(i).bits.vaddr := dataBuffer.io.deq(i).bits.vaddr
    io.sbuffer(i).bits.data  := dataBuffer.io.deq(i).bits.data
    io.sbuffer(i).bits.mask  := dataBuffer.io.deq(i).bits.mask
    io.sbuffer(i).bits.wline := dataBuffer.io.deq(i).bits.wline
    io.sbuffer(i).bits.id    := DontCare
    io.sbuffer(i).bits.instrtype    := DontCare
    io.sbuffer(i).bits.robIdx := DontCare

    // io.sbuffer(i).fire is RegNexted, as sbuffer data write takes 2 cycles.
    // Before data write finish, sbuffer is unable to provide store to load
    // forward data. As an workaround, deqPtrExt and allocated flag update 
    // is delayed so that load can get the right data from store queue.
    val ptr = dataBuffer.io.deq(i).bits.sqPtr.value
    when (RegNext(dataBuffer.io.deq(i).fire)) {
      allocated(RegEnable(ptr, dataBuffer.io.deq(i).fire)) := false.B
      XSDebug("sbuffer "+i+" fire: ptr %d\n", ptr)
    }
  }
  when (io.sbuffer(1).fire) {
    assert(io.sbuffer(0).fire)
  }
  if (coreParams.dcacheParametersOpt.isEmpty) {
    for (i <- 0 until StorePipelineWidth) {
      val ptr = deqPtrExt(i).value
      // val ram = DifftestMem(64L * 1024 * 1024 * 1024, 8)
      // val wen = allocated(ptr) && committed(ptr) && !mmio(ptr)
      // val waddr = ((paddrModule.io.rdata(i) - "h80000000".U) >> 3).asUInt
      // val wdata = Mux(paddrModule.io.rdata(i)(3), dataModule.io.rdata(i).data(127, 64), dataModule.io.rdata(i).data(63, 0))
      // val wmask = Mux(paddrModule.io.rdata(i)(3), dataModule.io.rdata(i).mask(15, 8), dataModule.io.rdata(i).mask(7, 0))
      // when (wen) {
      //   ram.write(waddr, wdata.asTypeOf(Vec(8, UInt(8.W))), wmask.asBools)
      // }
    }
  }

  // Read vaddr for mem exception
//  io.exceptionAddr.vaddr := v_pAddrModule.io.rdata_ext_v(0)

  // misprediction recovery / exception redirect
  // invalidate sq term using robIdx
  val needCancel = Wire(Vec(StoreQueueSize, Bool()))
  for (i <- 0 until StoreQueueSize) {
    needCancel(i) := uop(i).robIdx.needFlush(io.brqRedirect) && allocated(i) && !committed(i)
    when (needCancel(i)) {
      allocated(i) := false.B
      readyToDeq(i) := false.B
      readyToLeave(i) := false.B
    }
  }

  /**
    * update pointers
    */
  val lastEnqCancel = PopCount(RegNext(VecInit(canEnqueue.zip(enqCancel).map(x => x._1 && x._2))))
  val lastCycleRedirect = RegNext(io.brqRedirect.valid)
  val lastCycleCancelCount = PopCount(RegNext(needCancel))
  val enqNumber = Mux(io.enq.canAccept && io.enq.lqCanAccept, PopCount(io.enq.req.map(_.valid)), 0.U)
  val enqNumber_enq = Mux(io.enq.canAccept && io.enq.lqCanAccept, io.enq.reqNum, 0.U)

  when (lastCycleRedirect) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExt := VecInit(enqPtrExt.map(_ - (lastCycleCancelCount + lastEnqCancel)))
  }.otherwise {
    enqPtrExt := VecInit(enqPtrExt.map(_ + enqNumber_enq))
  }

  deqPtrExt := deqPtrExtNext
  rdataPtrExt := rdataPtrExtNext

  // val dequeueCount = Mux(io.sbuffer(1).fire, 2.U, Mux(io.sbuffer(0).fire || io.mmioStout.fire, 1.U, 0.U))

  // If redirect at T0, sqCancelCnt is at T2
  io.sqCancelCnt := RegNext(lastCycleCancelCount + lastEnqCancel)

  // io.sqempty will be used by sbuffer
  // We delay it for 1 cycle for better timing
  // When sbuffer need to check if it is empty, the pipeline is blocked, which means delay io.sqempty
  // for 1 cycle will also promise that sq is empty in that cycle
  io.sqempty := RegNext(
    enqPtrExt(0).value === deqPtrExt(0).value && 
    enqPtrExt(0).flag === deqPtrExt(0).flag
  )

  // perf counter
  QueuePerf(StoreQueueSize, validCount, !allowEnqueue)
  io.sqFull := !allowEnqueue
  XSPerfAccumulate("mmioCycle", (mmio_state =/= s_idle)) // lq is busy dealing with uncache req
  XSPerfAccumulate("mmioCnt", io.uncache.req.fire)
  XSPerfAccumulate("mmio_wb_success", io.mmioStout.fire)
  XSPerfAccumulate("mmio_wb_blocked", io.mmioStout.valid && !io.mmioStout.ready)
  XSPerfAccumulate("validEntryCnt", distanceBetween(enqPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("cmtEntryCnt", distanceBetween(cmtPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("nCmtEntryCnt", distanceBetween(enqPtrExt(0), cmtPtrExt(0)))

  val perfValidCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))
  val perfEvents = Seq(
    ("mmioCycle      ", (mmio_state =/= s_idle)),
    ("mmioCnt        ", io.uncache.req.fire),
    ("mmio_wb_success", io.mmioStout.fire),
    ("mmio_wb_blocked", io.mmioStout.valid && !io.mmioStout.ready),
    ("stq_1_4_valid  ", (perfValidCount < (StoreQueueSize.U/4.U))),
    ("stq_2_4_valid  ", (perfValidCount > (StoreQueueSize.U/4.U)) & (perfValidCount <= (StoreQueueSize.U/2.U))),
    ("stq_3_4_valid  ", (perfValidCount > (StoreQueueSize.U/2.U)) & (perfValidCount <= (StoreQueueSize.U*3.U/4.U))),
    ("stq_4_4_valid  ", (perfValidCount > (StoreQueueSize.U*3.U/4.U))),
  )
  generatePerfEvent()

  // debug info
  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtrExt(0).flag, enqPtr, deqPtrExt(0).flag, deqPtr)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until StoreQueueSize) {
    XSDebug(i + ": pc %x va %x pa %x data %x ",
      uop(i).cf.pc,
      debug_vaddr(i),
      debug_paddr(i),
      debug_data(i)
    )
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && addrvalid(i), "a")
    PrintFlag(allocated(i) && datavalid(i), "d")
    PrintFlag(allocated(i) && committed(i), "c")
    PrintFlag(allocated(i) && mmio(i), "m")
    XSDebug(false, true.B, "\n")
  }

}
