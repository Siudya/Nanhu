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

package xiangshan.frontend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xs.utils.mbist.MBISTPipeline
import utils._
import xs.utils._
import xiangshan._
import xiangshan.frontend.icache._
import xiangshan.backend.CtrlToFtqIO
import xiangshan.backend.decode.ImmUnion
import xs.utils.perf.HasPerfLogging
import xs.utils.sram.SRAMTemplate

class FtqPtr(implicit p: Parameters) extends CircularQueuePtr[FtqPtr](
  p => p(XSCoreParamsKey).FtqSize
){
}

object FtqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): FtqPtr = {
    val ptr = Wire(new FtqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: FtqPtr)(implicit p: Parameters): FtqPtr = {
    apply(!ptr.flag, ptr.value)
  }
}

class FtqNRSRAM[T <: Data](gen: T, numRead: Int, parentName:String = "Unknown")(implicit p: Parameters) extends XSModule {

  val io = IO(new Bundle() {
    val raddr = Input(Vec(numRead, UInt(log2Up(FtqSize).W)))
    val ren = Input(Vec(numRead, Bool()))
    val rdata = Output(Vec(numRead, gen))
    val waddr = Input(UInt(log2Up(FtqSize).W))
    val wen = Input(Bool())
    val wdata = Input(gen)
  })

  for(i <- 0 until numRead){
    val sram = Module(new SRAMTemplate(gen = gen, set = FtqSize,
      bypassWrite = true,
      hasMbist = coreParams.hasMbist,
      hasShareBus = coreParams.hasShareBus,
      parentName = parentName + s"sram${i}_"
    ))
    sram.io.r.req.valid := io.ren(i)
    sram.io.r.req.bits.setIdx := io.raddr(i)
    io.rdata(i) := sram.io.r.resp.data(0)
    sram.io.w.req.valid := io.wen
    sram.io.w.req.bits.setIdx := io.waddr
    sram.io.w.req.bits.data := VecInit(io.wdata)
  }
  val mbistPipeline = if(coreParams.hasMbist && coreParams.hasShareBus) {
    MBISTPipeline.PlaceMbistPipeline(1, s"${parentName}_mbistPipe")
  } else {
    None
  }
}

class FtqPCEntry(implicit p: Parameters) extends XSBundle with BPUUtils with HasBPUConst {
  val startAddr = UInt(VAddrBits.W)
  val nextLineAddr = UInt(VAddrBits.W)
  val isNextMask = Vec(PredictWidth, Bool())
  val fallThruError = Bool()
  // val carry = Bool()
  def getPc(offset: UInt) = {
    def getHigher(pc: UInt) = pc(VAddrBits-1, log2Ceil(PredictWidth)+instOffsetBits+1)
    def getOffset(pc: UInt) = pc(log2Ceil(PredictWidth)+instOffsetBits, instOffsetBits)
    Cat(getHigher(Mux(isNextMask(offset) && startAddr(log2Ceil(PredictWidth)+instOffsetBits), nextLineAddr, startAddr)),
        getOffset(startAddr)+offset, 0.U(instOffsetBits.W))
  }
  def fromBranchPrediction(resp: BranchPredictionBundle) = {
    def carryPos(addr: UInt) = addr(instOffsetBits+log2Ceil(PredictWidth)+1)
    this.startAddr := resp.pc(dupForFtq)
    this.nextLineAddr := resp.pc(dupForFtq) + (FetchWidth * 4 * 2).U // may be broken on other configs
    this.isNextMask := VecInit((0 until PredictWidth).map(i =>
      (resp.pc(dupForFtq)(log2Ceil(PredictWidth), 1) +& i.U)(log2Ceil(PredictWidth)).asBool
    ))
    this.fallThruError := resp.fallThruError(dupForFtq)
    this
  }
  override def toPrintable: Printable = {
    p"startAddr:${Hexadecimal(startAddr)}"
  }
}

class FtqPdEntry(implicit p: Parameters) extends XSBundle {
  val brMask = Vec(PredictWidth, Bool())
  val jmpInfo = ValidUndirectioned(Vec(3, Bool()))
  val jmpOffset = UInt(log2Ceil(PredictWidth).W)
  val jalTarget = UInt(VAddrBits.W)
  val rvcMask = Vec(PredictWidth, Bool())
  def hasJal  = jmpInfo.valid && !jmpInfo.bits(0)
  def hasJalr = jmpInfo.valid && jmpInfo.bits(0)
  def hasCall = jmpInfo.valid && jmpInfo.bits(1)
  def hasRet  = jmpInfo.valid && jmpInfo.bits(2)

  def fromPdWb(pdWb: PredecodeWritebackBundle) = {
    val pds = pdWb.pd
    this.brMask := VecInit(pds.map(pd => pd.isBr && pd.valid))
    this.jmpInfo.valid := VecInit(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid)).asUInt.orR
    this.jmpInfo.bits := ParallelPriorityMux(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid),
                                             pds.map(pd => VecInit(pd.isJalr, pd.isCall, pd.isRet)))
    this.jmpOffset := ParallelPriorityEncoder(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid))
    this.rvcMask := VecInit(pds.map(pd => pd.isRVC))
    this.jalTarget := pdWb.jalTarget
  }

  def toPd(offset: UInt) = {
    require(offset.getWidth == log2Ceil(PredictWidth))
    val pd = Wire(new PreDecodeInfo)
    pd.valid := true.B
    pd.isRVC := rvcMask(offset)
    val isBr = brMask(offset)
    val isJalr = offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(0)
    pd.brType := Cat(offset === jmpOffset && jmpInfo.valid, isJalr || isBr)
    pd.isCall := offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(1)
    pd.isRet  := offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(2)
    pd
  }
}

class FtqRedirectEntry(implicit p: Parameters) extends SpeculativeInfo {}

class FtqMetaEntry(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val meta = UInt(MaxMetaLength.W)
}


class FtqToBpuIO(implicit p: Parameters) extends XSBundle {
  val redirect = Valid(new BranchPredictionRedirect)
  val update = Valid(new BranchPredictionUpdate)
  val enq_ptr = Output(new FtqPtr)
}

class FtqToIfuIO(implicit p: Parameters) extends XSBundle with HasCircularQueuePtrHelper {
  val req = Decoupled(new FetchRequestBundle)
  val redirect = Valid(new Redirect)
  val flushFromBpu = new Bundle {
    // when ifu pipeline is not stalled,
    // a packet from bpu s3 can reach f1 at most
    val s2 = Valid(new FtqPtr)
    val s3 = Valid(new FtqPtr)
    def shouldFlushBy(src: Valid[FtqPtr], idx_to_flush: FtqPtr) = {
      src.valid && !isAfter(src.bits, idx_to_flush)
    }
    def shouldFlushByStage2(idx: FtqPtr) = shouldFlushBy(s2, idx)
    def shouldFlushByStage3(idx: FtqPtr) = shouldFlushBy(s3, idx)
  }
}

class FtqToICacheIO(implicit p: Parameters) extends XSBundle with HasCircularQueuePtrHelper {
  //NOTE: req.bits must be prepare in T cycle
  // while req.valid is set true in T + 1 cycle
  val req = Decoupled(new FtqToICacheRequestBundle)
}

class FtqToCtrlIO(implicit p: Parameters) extends XSBundle {
  // write to backend pc mem
  val pc_mem_wen = Output(Bool())
  val pc_mem_waddr = Output(UInt(log2Ceil(FtqSize).W))
  val pc_mem_wdata = Output(new FtqPCEntry)
  val safeTargetPtr = Output(new FtqPtr)
}


class FTBEntryGen(implicit p: Parameters) extends XSModule with HasBPUParameter {
  val io = IO(new Bundle {
    val start_addr = Input(UInt(VAddrBits.W))
    val oldEntry = Input(new FTBEntry)
    val pd = Input(new FtqPdEntry)
    val cfiIndex = Flipped(Valid(UInt(log2Ceil(PredictWidth).W)))
    val target = Input(UInt(VAddrBits.W))
    val hit = Input(Bool())
    val mispredictVec = Input(Vec(PredictWidth, Bool()))

    val new_entry = Output(new FTBEntry)
    val new_br_insert_pos = Output(Bool())
    val taken_mask = Output(Bool())
    val jmp_taken = Output(Bool())
    val mispred_mask = Output(Vec(1+1, Bool()))

    // for perf counters
    val is_init_entry = Output(Bool())
    val is_old_entry = Output(Bool())
    val is_new_br = Output(Bool())
    val is_jalr_target_modified = Output(Bool())
    val is_always_taken_modified = Output(Bool())
    val is_br_full = Output(Bool())
  })

  // no mispredictions detected at predecode
  val hit = io.hit
  val pd = io.pd

  val init_entry = WireInit(0.U.asTypeOf(new FTBEntry))


  val cfi_is_br = pd.brMask(io.cfiIndex.bits) && io.cfiIndex.valid
  val entry_has_jmp = pd.jmpInfo.valid
  val new_jmp_is_jal  = entry_has_jmp && !pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_jalr = entry_has_jmp &&  pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_call = entry_has_jmp &&  pd.jmpInfo.bits(1) && io.cfiIndex.valid
  val new_jmp_is_ret  = entry_has_jmp &&  pd.jmpInfo.bits(2) && io.cfiIndex.valid
  val last_jmp_rvi = entry_has_jmp && pd.jmpOffset === (PredictWidth-1).U && !pd.rvcMask.last
  // val last_br_rvi = cfi_is_br && io.cfiIndex.bits === (PredictWidth-1).U && !pd.rvcMask.last

  val cfi_is_jal = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jal
  val cfi_is_jalr = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jalr

  def carryPos = log2Ceil(PredictWidth)+instOffsetBits
  def getLower(pc: UInt) = pc(carryPos-1, instOffsetBits)
  // if not hit, establish a new entry
  init_entry.valid := true.B
  // tag is left for ftb to assign

  // case br
  when (cfi_is_br) {
    init_entry.offset := io.cfiIndex.bits
    init_entry.setLowerStatByTarget(io.start_addr, io.target, isShare = true)
    init_entry.alwaysTaken := true.B // set to always taken on init
  }

  // case jmp
  when (entry_has_jmp) {
    init_entry.offset := pd.jmpOffset
    init_entry.setLowerStatByTarget(io.start_addr, Mux(cfi_is_jalr, io.target, pd.jalTarget))
  }

  val jmpPft = getLower(io.start_addr) +& pd.jmpOffset +& Mux(pd.rvcMask(pd.jmpOffset), 1.U, 2.U)
  init_entry.pftAddr := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft, getLower(io.start_addr))
  init_entry.carry   := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft(carryPos-instOffsetBits), true.B)
  init_entry.isJalr := new_jmp_is_jalr
  init_entry.isCall := new_jmp_is_call
  init_entry.isRet  := new_jmp_is_ret
  // that means fall thru points to the middle of an inst
  init_entry.last_may_be_rvi_call := pd.jmpOffset === (PredictWidth-1).U && !pd.rvcMask(pd.jmpOffset)

  // if hit, check whether a new cfi(only br is possible) is detected
  val oe = io.oldEntry
  val br_recorded = oe.brIsRecorded(io.cfiIndex.bits)
  val is_new_br = cfi_is_br && !br_recorded
  val new_br_offset = io.cfiIndex.bits

  /** slot empty or lower branch */
  val new_br_insert_onehot: Bool = new_br_offset < oe.offset

  val old_entry_modified = WireInit(io.oldEntry)
  when (new_br_insert_onehot) {
    old_entry_modified.valid := true.B
    old_entry_modified.offset := new_br_offset
    old_entry_modified.setLowerStatByTarget(io.start_addr, io.target, isShare = true)
    old_entry_modified.alwaysTaken := true.B
  }.elsewhen (new_br_offset > oe.offset) {
    old_entry_modified.alwaysTaken := false.B
    // all other fields remain unchanged
  }

  // two circumstances:
  // 1. oe: | br | j  |, new br should be in front of j, thus addr of j should be new pft
  // 2. oe: | br | br |, new br could be anywhere between, thus new pft is the addr of either
  //        the previous last br or the new br
  val may_have_to_replace = oe.noEmptySlotForNewBr
  val pft_need_to_change = is_new_br && may_have_to_replace
  // it should either be the given last br or the new br
  when (pft_need_to_change) {
    val new_pft_offset =
      Mux(!new_br_insert_onehot,
        new_br_offset, oe.offset)

    // set jmp to invalid
    old_entry_modified.pftAddr := getLower(io.start_addr) + new_pft_offset
    old_entry_modified.carry := (getLower(io.start_addr) +& new_pft_offset).head(1).asBool
    old_entry_modified.last_may_be_rvi_call := false.B
    old_entry_modified.isCall := false.B
    old_entry_modified.isRet := false.B
    old_entry_modified.isJalr := false.B
  }

  val old_entry_jmp_target_modified = WireInit(oe)
  val old_target = oe.getTarget(io.start_addr) // may be wrong because we store only 20 lowest bits
  val old_tail_is_jmp = !oe.sharing
  val jalr_target_modified = cfi_is_jalr && (old_target =/= io.target) && old_tail_is_jmp // TODO: pass full jalr target
  when (jalr_target_modified) {
    old_entry_jmp_target_modified.setLowerStatByTarget(io.start_addr, io.target)
    old_entry_jmp_target_modified.alwaysTaken := 0.U.asTypeOf(Bool())
  }

  val old_entry_always_taken = WireInit(oe)
  val always_taken_modified = Wire(Bool()) // whether modified or not
  old_entry_always_taken.alwaysTaken :=
    oe.alwaysTaken && io.cfiIndex.valid && oe.brValid && io.cfiIndex.bits === oe.offset
  always_taken_modified := oe.alwaysTaken && !old_entry_always_taken.alwaysTaken

  val derived_from_old_entry =
    Mux(is_new_br, old_entry_modified,
      Mux(jalr_target_modified, old_entry_jmp_target_modified, old_entry_always_taken))

  io.new_entry := Mux(!hit, init_entry, derived_from_old_entry)

  io.new_br_insert_pos := new_br_insert_onehot
  io.taken_mask := io.cfiIndex.bits === io.new_entry.offset && io.cfiIndex.valid && io.new_entry.brValid

  io.jmp_taken := io.new_entry.jmpValid && io.new_entry.offset === io.cfiIndex.bits

  /** mispred_mask head means branch mistake,
   *  mispred_mask last means jump mistake
   */
  io.mispred_mask.head := io.new_entry.brValid && io.mispredictVec(io.new_entry.offset)
  io.mispred_mask.last := io.new_entry.jmpValid && io.mispredictVec(pd.jmpOffset)

  // for perf counters
  io.is_init_entry := !hit
  io.is_old_entry := hit && !is_new_br && !jalr_target_modified && !always_taken_modified
  io.is_new_br := hit && is_new_br
  io.is_jalr_target_modified := hit && jalr_target_modified
  io.is_always_taken_modified := hit && always_taken_modified
  io.is_br_full := hit && is_new_br && may_have_to_replace
}

class FtqPcMemWrapper(numOtherReads: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ifuPtr_w       = Input(new FtqPtr)
    val ifuPtrPlus1_w  = Input(new FtqPtr)
    val ifuPtrPlus2_w  = Input(new FtqPtr)
    val commPtr_w      = Input(new FtqPtr)
    val commPtrPlus1_w = Input(new FtqPtr)
    val ifuPtr_rdata       = Output(new FtqPCEntry)
    val ifuPtrPlus1_rdata  = Output(new FtqPCEntry)
    val ifuPtrPlus2_rdata  = Output(new FtqPCEntry)
    val commPtr_rdata      = Output(new FtqPCEntry)
    val commPtrPlus1_rdata = Output(new FtqPCEntry)

    val other_raddrs = Input(Vec(numOtherReads, UInt(log2Ceil(FtqSize).W)))
    val other_rdatas = Output(Vec(numOtherReads, new FtqPCEntry))

    val wen = Input(Bool())
    val waddr = Input(UInt(log2Ceil(FtqSize).W))
    val wdata = Input(new FtqPCEntry)
  })

  val num_pc_read = numOtherReads + 5
  val mem = Module(new SyncDataModuleTemplate(new FtqPCEntry, FtqSize,
    num_pc_read, 1, "FtqPC"))
  mem.io.wen(0)   := io.wen
  mem.io.waddr(0) := io.waddr
  mem.io.wdata(0) := io.wdata

  // read one cycle ahead for ftq local reads
  val raddr_vec = VecInit(io.other_raddrs ++
    Seq(io.ifuPtr_w.value, io.ifuPtrPlus1_w.value, io.ifuPtrPlus2_w.value, io.commPtrPlus1_w.value, io.commPtr_w.value))

  mem.io.raddr := raddr_vec

  io.other_rdatas       := mem.io.rdata.dropRight(5)
  io.ifuPtr_rdata       := mem.io.rdata.dropRight(4).last
  io.ifuPtrPlus1_rdata  := mem.io.rdata.dropRight(3).last
  io.ifuPtrPlus2_rdata  := mem.io.rdata.dropRight(2).last
  io.commPtrPlus1_rdata := mem.io.rdata.dropRight(1).last
  io.commPtr_rdata      := mem.io.rdata.last
}

class Ftq(parentName:String = "Unknown")(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
  with BPUUtils with HasBPUConst with HasPerfEvents with HasPerfLogging
  with HasICacheParameters{
  val io = IO(new Bundle {
    val fromBpu = Flipped(new BpuToFtqIO)
    val fromIfu = Flipped(new IfuToFtqIO)
    val fromBackend = Flipped(new CtrlToFtqIO)

    val toBpu = new FtqToBpuIO
    val toIfu = new FtqToIfuIO
    val toICache = new FtqToICacheIO
    val toBackend = new FtqToCtrlIO
    val toIbuffer = Valid(new FtqPtr)

    val toPrefetch = new FtqPrefechBundle

    val bpuInfo = new Bundle {
      val bpRight = Output(UInt(XLEN.W))
      val bpWrong = Output(UInt(XLEN.W))
    }

    val mmioCommitRead = Flipped(new mmioCommitRead)
  })
  io.bpuInfo := DontCare

  val backendRedirect = Wire(Valid(new Redirect))
  val backendRedirectReg = RegNext(backendRedirect)

  val stage2Flush = backendRedirect.valid
  val backendFlush = stage2Flush || RegNext(stage2Flush)
  val ifuFlush = Wire(Bool())

  val flush = stage2Flush || RegNext(stage2Flush)

  val allowBpuIn, allowToIfu = WireInit(false.B)
  val flushToIfu = !allowToIfu
  allowBpuIn := !ifuFlush && !backendRedirect.valid && !backendRedirectReg.valid
  allowToIfu := !ifuFlush && !backendRedirect.valid && !backendRedirectReg.valid

  def copyNum = 5
  val bpuPtr, ifuPtr, ifuWbPtr, commPtr = RegInit(FtqPtr(false.B, 0.U))
  val ifuPtrPlus1 = RegInit(FtqPtr(false.B, 1.U))
  val ifuPtrPlus2 = RegInit(FtqPtr(false.B, 2.U))
  val commPtrPlus1 = RegInit(FtqPtr(false.B, 1.U))
  val copied_ifu_ptr = Seq.fill(copyNum)(RegInit(FtqPtr(false.B, 0.U)))
  val copied_bpu_ptr = Seq.fill(copyNum)(RegInit(FtqPtr(false.B, 0.U)))
  require(FtqSize >= 4)
  val ifuPtr_write       = WireInit(ifuPtr)
  val ifuPtrPlus1_write  = WireInit(ifuPtrPlus1)
  val ifuPtrPlus2_write  = WireInit(ifuPtrPlus2)
  val ifuWbPtr_write     = WireInit(ifuWbPtr)
  val commPtr_write      = WireInit(commPtr)
  val commPtrPlus1_write = WireInit(commPtrPlus1)
  ifuPtr       := ifuPtr_write
  ifuPtrPlus1  := ifuPtrPlus1_write
  ifuPtrPlus2  := ifuPtrPlus2_write
  ifuWbPtr     := ifuWbPtr_write
  commPtr      := commPtr_write
  commPtrPlus1 := commPtrPlus1_write
  copied_ifu_ptr.map{ptr =>
    ptr := ifuPtr_write
    dontTouch(ptr)
  }
  val validEntries = distanceBetween(bpuPtr, commPtr)

  /** BPU TO FTQ
   * BPU select one from three predictions generated by three stage, which have same interface.
   * Stage 1: Prediction stash PC info into [[ftqPcMem]] and register a ftqPtr([[bpuPtr]])
   * Stage 2 and Stage 3: This prediction indicate a mis-prediction at preceding prediction,
   *                      which cause a redirect and flush ifu.
   * Stage 3 prediction stash(initial) all info in to FTQ such as FTB entry, Meta, etc.
   * */

  val canCommit = Wire(Bool()) //#2034
  val newEntryReady = validEntries < FtqSize.U || canCommit
  io.fromBpu.resp.ready := newEntryReady

  val bpus2Resp = io.fromBpu.resp.bits.s2
  val bpuS3Resp = io.fromBpu.resp.bits.s3
  val bpuS2Redirect = bpus2Resp.valid(dupForFtq) && bpus2Resp.hasRedirect(dupForFtq)
  val bpuS3Redirect = bpuS3Resp.valid(dupForFtq) && bpuS3Resp.hasRedirect(dupForFtq)

  io.toBpu.enq_ptr := bpuPtr
  val enqFire = io.fromBpu.resp.fire && allowBpuIn // from bpu s1
  val bpuInFire = (io.fromBpu.resp.fire || bpuS2Redirect || bpuS3Redirect) && allowBpuIn

  val bpuInResp = io.fromBpu.resp.bits.selectedRespForFtq
  val bpuInStage = io.fromBpu.resp.bits.selectedRespIdxForFtq
  val bpuInRespPtr = Mux(bpuInStage === BP_S1, bpuPtr, bpuInResp.ftqIdx)
  val bpuInRespIdx = bpuInRespPtr.value

  // read ports:      prefetchReq ++  ifuReq1 + ifuReq2 + ifuReq3 + commitUpdate2 + commitUpdate
  val ftqPcMem = Module(new FtqPcMemWrapper(1))
  // resp from uBTB
  ftqPcMem.io.wen := bpuInFire
  ftqPcMem.io.waddr := bpuInRespIdx
  ftqPcMem.io.wdata.fromBranchPrediction(bpuInResp)

  //                                                            ifuRedirect + backendRedirect + commit
  val ftqRedirectSram = Module(new FtqNRSRAM(new FtqRedirectEntry, 1+1+1, parentName = parentName + s"ftq_redirect_sram_"))
  // these info is intended to enq at the last stage of bpu
  ftqRedirectSram.io.wen := io.fromBpu.resp.bits.lastStage.valid(dupForFtq)
  ftqRedirectSram.io.waddr := io.fromBpu.resp.bits.lastStage.ftqIdx.value
  ftqRedirectSram.io.wdata := io.fromBpu.resp.bits.lastStageSpecInfo
  println(f"ftq redirect SRAM: entry ${ftqRedirectSram.io.wdata.getWidth} * ${FtqSize} * 3")
  println(f"ftq redirect SRAM: ahead fh ${ftqRedirectSram.io.wdata.afhob.getWidth} * ${FtqSize} * 3")

  val ftqMetaSram = Module(new FtqNRSRAM(new FtqMetaEntry, 1, parentName = parentName + s"ftq_meta_1r_sram_"))
  // these info is intended to enq at the last stage of bpu
  ftqMetaSram.io.wen := io.fromBpu.resp.bits.lastStage.valid(dupForFtq)
  ftqMetaSram.io.waddr := io.fromBpu.resp.bits.lastStage.ftqIdx.value
  ftqMetaSram.io.wdata.meta := io.fromBpu.resp.bits.lastStageMeta

  val mbistPipeline = if(coreParams.hasMbist && coreParams.hasShareBus) {
    MBISTPipeline.PlaceMbistPipeline(2, s"${parentName}_mbistPipe")
  } else {
    None
  }

  //                                                            ifuRedirect + backendRedirect + commit
  val ftbEntryMem = Module(new SyncDataModuleTemplate(new FTBEntry, FtqSize, 1+1+1, 1, "FtqEntry"))
  ftbEntryMem.io.wen(0) := io.fromBpu.resp.bits.lastStage.valid(dupForFtq)
  ftbEntryMem.io.waddr(0) := io.fromBpu.resp.bits.lastStage.ftqIdx.value
  ftbEntryMem.io.wdata(0) := io.fromBpu.resp.bits.lastStageFtbEntry


  // multi-write
  val updateTarget = Reg(Vec(FtqSize, UInt(VAddrBits.W))) // could be taken target or fallThrough //TODO: remove this
  val newestEntryTarget = Reg(UInt(VAddrBits.W))
  val newestEntryPtr = Reg(new FtqPtr)
  val cfiIndexVec = Reg(Vec(FtqSize, ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  val mispredictVec = Reg(Vec(FtqSize, Vec(PredictWidth, Bool())))
  val predStage = Reg(Vec(FtqSize, UInt(2.W)))

  val c_invalid :: c_valid :: c_commited :: Nil = Enum(3)
  val commitStateQueue = RegInit(VecInit(Seq.fill(FtqSize) {
    VecInit(Seq.fill(PredictWidth)(c_invalid))
  }))

  val f_to_send :: f_sent :: Nil = Enum(2)
  val entryFetchStatus = RegInit(VecInit(Seq.fill(FtqSize)(f_sent)))

  val h_not_hit :: h_false_hit :: h_hit :: Nil = Enum(3)
  val entryHitStatus = RegInit(VecInit(Seq.fill(FtqSize)(h_not_hit)))

  // modify registers one cycle later to cut critical path
  val lastCycleBpuIn = RegNext(bpuInFire)
  val lastCycleBpuInPtr = RegNext(bpuInRespPtr)
  val lastCycleBpuInIdx = lastCycleBpuInPtr.value
  val lastCycleBpuTarget = RegNext(bpuInResp.target(dupForFtq))
  val lastCycleCfiIndex = RegNext(bpuInResp.cfiIndex(dupForFtq))
  val lastCycleBpuInStage = RegNext(bpuInStage)

  def extra_copyNum_for_commitStateQueue = 2
  val copied_last_cycle_bpu_in = VecInit(Seq.fill(copyNum+extra_copyNum_for_commitStateQueue)(RegNext(bpuInFire)))
  val copied_last_cycle_bpu_in_ptr_for_ftq = VecInit(Seq.fill(extra_copyNum_for_commitStateQueue)(RegNext(bpuInRespPtr)))

  when (lastCycleBpuIn) {
    entryFetchStatus(lastCycleBpuInIdx) := f_to_send
    cfiIndexVec(lastCycleBpuInIdx) := lastCycleCfiIndex
    predStage(lastCycleBpuInIdx) := lastCycleBpuInStage

    updateTarget(lastCycleBpuInIdx) := lastCycleBpuTarget // TODO: remove this
    newestEntryTarget := lastCycleBpuTarget
    newestEntryPtr := lastCycleBpuInPtr
  }

  // reduce fanout by delay write for a cycle
  when (RegNext(lastCycleBpuIn)) {
    mispredictVec(RegNext(lastCycleBpuInIdx)) := WireInit(VecInit(Seq.fill(PredictWidth)(false.B)))
  }

  // reduce fanout using copied lastCycleBpuIn and copied lastCycleBpuInPtr
  val copied_last_cycle_bpu_in_for_ftq = copied_last_cycle_bpu_in.takeRight(extra_copyNum_for_commitStateQueue)
  copied_last_cycle_bpu_in_for_ftq.zip(copied_last_cycle_bpu_in_ptr_for_ftq).zipWithIndex.map {
    case ((in, ptr), i) =>
      when (in) {
        val perSetEntries = FtqSize / extra_copyNum_for_commitStateQueue // 32
        require(FtqSize % extra_copyNum_for_commitStateQueue == 0)
        for (j <- 0 until perSetEntries) {
          when (ptr.value === (i*perSetEntries + j).U) {
            commitStateQueue(i*perSetEntries + j) := VecInit(Seq.fill(PredictWidth)(c_invalid))
          }
        }
      }
  }

  // num cycle is fixed
  io.toBackend.safeTargetPtr := ifuWbPtr

  bpuPtr := bpuPtr + enqFire
  copied_bpu_ptr.foreach(_ := bpuPtr + enqFire)
  io.toIbuffer.bits := bpuPtr + enqFire
  io.toIbuffer.valid := enqFire
  when (io.toIfu.req.fire && allowToIfu) {
    ifuPtr_write := ifuPtrPlus1
    ifuPtrPlus1_write := ifuPtrPlus2
    ifuPtrPlus2_write := ifuPtrPlus2 + 1.U
  }

  // only use ftb result to assign hit status
  when (bpus2Resp.valid(dupForFtq)) {
    entryHitStatus(bpus2Resp.ftqIdx.value) := Mux(bpus2Resp.fullPred(dupForFtq).hit, h_hit, h_not_hit)
  }

  io.toIfu.flushFromBpu.s2.valid := bpuS2Redirect
  io.toIfu.flushFromBpu.s2.bits := bpus2Resp.ftqIdx
  when (bpuS2Redirect) {
    bpuPtr := bpus2Resp.ftqIdx + 1.U
    io.toIbuffer.bits := bpus2Resp.ftqIdx + 1.U
    io.toIbuffer.valid := true.B
    copied_bpu_ptr.foreach(_ := bpus2Resp.ftqIdx + 1.U)
    // only when ifuPtr runs ahead of bpu s2 resp should we recover it
    when (!isBefore(ifuPtr, bpus2Resp.ftqIdx)) {
      ifuPtr_write := bpus2Resp.ftqIdx
      ifuPtrPlus1_write := bpus2Resp.ftqIdx + 1.U
      ifuPtrPlus2_write := bpus2Resp.ftqIdx + 2.U
    }
  }

  io.toIfu.flushFromBpu.s3.valid := bpuS3Redirect
  io.toIfu.flushFromBpu.s3.bits := bpuS3Resp.ftqIdx
  when (bpuS3Redirect) {
    bpuPtr := bpuS3Resp.ftqIdx + 1.U
    io.toIbuffer.valid := true.B
    io.toIbuffer.bits := bpuS3Resp.ftqIdx + 1.U
    copied_bpu_ptr.foreach(_ := bpuS3Resp.ftqIdx + 1.U)
    // only when ifuPtr runs ahead of bpu s2 resp should we recover it
    when (!isBefore(ifuPtr, bpuS3Resp.ftqIdx)) {
      ifuPtr_write := bpuS3Resp.ftqIdx
      ifuPtrPlus1_write := bpuS3Resp.ftqIdx + 1.U
      ifuPtrPlus2_write := bpuS3Resp.ftqIdx + 2.U
    }
  }

  XSError(isBefore(bpuPtr, ifuPtr) && !isFull(bpuPtr, ifuPtr), "\nifuPtr is before bpuPtr!\n")

  (0 until copyNum).map{i =>
    XSError(copied_bpu_ptr(i) =/= bpuPtr, "\ncopiedBpuPtr is different from bpuPtr!\n")
  }

  /** FTQ TO IFU/ICache
   * FTQ send fetch info to IFU and Icache. TODO: reconsider register duplication.
   * Three fetch candidates:
   * 1. Bpu enqueue bypass and has same pointer as ifuPtr; send bypass entry.
   * 2. Ifu fire at prev cycle; send entry at ifuPtr+1.
   * 3. Otherwise; send entry at ifuPtr.
   */
  // 0  for ifu, and 1-4 for ICache
  val bpuInBypass           = RegEnable(ftqPcMem.io.wdata, bpuInFire)
  val bpuInBypassPtr        = RegNext(bpuInRespPtr)
  val lastCycleToIfuFire    = RegNext(io.toIfu.req.fire)
  val bpuInBypassDup        = VecInit(Seq.fill(copyNum)(RegEnable(ftqPcMem.io.wdata, bpuInFire)))
  val bpuInBypassPtrDup     = VecInit(Seq.fill(copyNum)(RegNext(bpuInRespPtr)))
  val lastCycleToIfuFireDup = VecInit(Seq.fill(copyNum)(RegNext(io.toIfu.req.fire)))

  // read pc and target
  ftqPcMem.io.ifuPtr_w       := ifuPtr_write
  ftqPcMem.io.ifuPtrPlus1_w  := ifuPtrPlus1_write
  ftqPcMem.io.ifuPtrPlus2_w  := ifuPtrPlus2_write
  ftqPcMem.io.commPtr_w      := commPtr_write
  ftqPcMem.io.commPtrPlus1_w := commPtrPlus1_write

  io.toIfu.req.bits.ftqIdx := ifuPtr

  val toICachePcBundle    = Wire(Vec(copyNum,new FtqPCEntry))
  val toICacheEntryToSend = Wire(Vec(copyNum,Bool()))
  val toIfuPcBundle       = Wire(new FtqPCEntry)
  val entryIsToSend       = WireInit(entryFetchStatus(ifuPtr.value) === f_to_send)
  val entryFtqOffset      = WireInit(cfiIndexVec(ifuPtr.value))
  val entryNextAddr       = Wire(UInt(VAddrBits.W))

  val pc_mem_ifu_ptr_rdata   = VecInit(Seq.fill(copyNum)(RegNext(ftqPcMem.io.ifuPtr_rdata)))
  val pc_mem_ifu_plus1_rdata = VecInit(Seq.fill(copyNum)(RegNext(ftqPcMem.io.ifuPtrPlus1_rdata)))
  val diff_entry_next_addr = WireInit(updateTarget(ifuPtr.value)) //TODO: remove this

  val copied_ifu_plus1_to_send = VecInit(Seq.fill(copyNum)(RegNext(entryFetchStatus(ifuPtrPlus1.value) === f_to_send) || RegNext(lastCycleBpuIn && bpuInBypassPtr === (ifuPtrPlus1))))
  val copied_ifu_ptr_to_send   = VecInit(Seq.fill(copyNum)(RegNext(entryFetchStatus(ifuPtr.value) === f_to_send) || RegNext(lastCycleBpuIn && bpuInBypassPtr === ifuPtr)))

  for(i <- 0 until copyNum){
    when(copied_last_cycle_bpu_in(i) && bpuInBypassPtrDup(i) === copied_ifu_ptr(i)){
      toICachePcBundle(i)    := bpuInBypassDup(i)
      toICacheEntryToSend(i) := true.B
    }.elsewhen(lastCycleToIfuFireDup(i)){
      toICachePcBundle(i)    := pc_mem_ifu_plus1_rdata(i)
      toICacheEntryToSend(i) := copied_ifu_plus1_to_send(i)
    }.otherwise{
      toICachePcBundle(i)    := pc_mem_ifu_ptr_rdata(i)
      toICacheEntryToSend(i) := copied_ifu_ptr_to_send(i)
    }
  }

  // TODO: reconsider target address bypass logic
  when (lastCycleBpuIn && bpuInBypassPtr === ifuPtr) {
    toIfuPcBundle := bpuInBypass
    entryIsToSend := true.B
    entryNextAddr := lastCycleBpuTarget
    entryFtqOffset := lastCycleCfiIndex
    diff_entry_next_addr := lastCycleBpuTarget // TODO: remove this
  }.elsewhen (lastCycleToIfuFire) {
    toIfuPcBundle := RegNext(ftqPcMem.io.ifuPtrPlus1_rdata)
    entryIsToSend := RegNext(entryFetchStatus(ifuPtrPlus1.value) === f_to_send) ||
                        RegNext(lastCycleBpuIn && bpuInBypassPtr === ifuPtrPlus1) // reduce potential bubbles
    entryNextAddr := Mux(lastCycleBpuIn && bpuInBypassPtr === ifuPtrPlus1,
                          bpuInBypass.startAddr,
                          Mux(ifuPtr === newestEntryPtr,
                            newestEntryTarget,
                            RegNext(ftqPcMem.io.ifuPtrPlus2_rdata.startAddr))) // ifuPtr+2
  }.otherwise {
    toIfuPcBundle := RegNext(ftqPcMem.io.ifuPtr_rdata)
    entryIsToSend := RegNext(entryFetchStatus(ifuPtr.value) === f_to_send) ||
                        RegNext(lastCycleBpuIn && bpuInBypassPtr === ifuPtr) // reduce potential bubbles
    entryNextAddr := Mux(lastCycleBpuIn && bpuInBypassPtr === ifuPtrPlus1,
                          bpuInBypass.startAddr,
                          Mux(ifuPtr === newestEntryPtr,
                            newestEntryTarget,
                            RegNext(ftqPcMem.io.ifuPtrPlus1_rdata.startAddr))) // ifuPtr+1
  }

  io.toIfu.req.valid := entryIsToSend && ifuPtr =/= bpuPtr
  io.toIfu.req.bits.nextStartAddr := entryNextAddr
  io.toIfu.req.bits.ftqOffset := entryFtqOffset
  io.toIfu.req.bits.fromFtqPcBundle(toIfuPcBundle)

  io.toICache.req.valid := entryIsToSend && ifuPtr =/= bpuPtr
  io.toICache.req.bits.readValid.zipWithIndex.foreach{case(copy, i) => copy := toICacheEntryToSend(i) && copied_ifu_ptr(i) =/= copied_bpu_ptr(i)}
  io.toICache.req.bits.pcMemRead.zipWithIndex.map{case(copy,i) => copy.fromFtqPcBundle(toICachePcBundle(i))}


  // TODO: remove this
  XSError(io.toIfu.req.valid && diff_entry_next_addr =/= entryNextAddr,
          p"\nifu_req_target wrong! ifuPtr: ${ifuPtr}, entryNextAddr: ${Hexadecimal(entryNextAddr)} diff_entry_next_addr: ${Hexadecimal(diff_entry_next_addr)}\n")

  // when fall through is smaller in value than start address, there must be a false hit
  when (toIfuPcBundle.fallThruError && entryHitStatus(ifuPtr.value) === h_hit) {
    when (io.toIfu.req.fire &&
      !(bpuS2Redirect && bpus2Resp.ftqIdx === ifuPtr) &&
      !(bpuS3Redirect && bpuS3Resp.ftqIdx === ifuPtr)
    ) {
      entryHitStatus(ifuPtr.value) := h_false_hit
    }
    XSDebug(true.B, "fallThruError! start:%x, fallThru:%x\n", io.toIfu.req.bits.startAddr, io.toIfu.req.bits.nextStartAddr)
  }

  XSPerfAccumulate(f"fall_through_error_to_ifu", toIfuPcBundle.fallThruError && entryHitStatus(ifuPtr.value) === h_hit &&
    io.toIfu.req.fire && !(bpuS2Redirect && bpus2Resp.ftqIdx === ifuPtr) && !(bpuS3Redirect && bpuS3Resp.ftqIdx === ifuPtr))

  val ifu_req_should_be_flushed =
    io.toIfu.flushFromBpu.shouldFlushByStage2(io.toIfu.req.bits.ftqIdx) ||
    io.toIfu.flushFromBpu.shouldFlushByStage3(io.toIfu.req.bits.ftqIdx)

    when (io.toIfu.req.fire && !ifu_req_should_be_flushed) {
      entryFetchStatus(ifuPtr.value) := f_sent
    }

  /** IFU TO FTQ
   * IFU send pre-decode info to ftq and Update stash info
   */
  val pdWb = io.fromIfu.pdWb
  val pds = pdWb.bits.pd
  val ifuWbValid = pdWb.valid
  val ifuWbIdx = pdWb.bits.ftqIdx.value
  // read ports:                                                         commit update
  val ftqPdMem = Module(new SyncDataModuleTemplate(new FtqPdEntry, FtqSize, 1, 1, "FtqPd"))
  ftqPdMem.io.wen.head := ifuWbValid
  ftqPdMem.io.waddr.head := pdWb.bits.ftqIdx.value
  ftqPdMem.io.wdata.head.fromPdWb(pdWb.bits)

  val hitPdValid = entryHitStatus(ifuWbIdx) === h_hit && ifuWbValid
  val hitPdMispred = hitPdValid && pdWb.bits.misOffset.valid
  val hitPdMispredReg = RegNext(hitPdMispred, init=false.B)
  val pdReg      = RegEnable(pds,             pdWb.valid)
  val startPcReg = RegEnable(pdWb.bits.pc(0), pdWb.valid)
  val wbIdxReg   = RegEnable(ifuWbIdx,      pdWb.valid)

  when (ifuWbValid) {
    val comm_stq_wen = VecInit(pds.map(_.valid).zip(pdWb.bits.instrRange).map{
      case (v, inRange) => v && inRange
    })
    (commitStateQueue(ifuWbIdx) zip comm_stq_wen).map{
      case (qe, v) => when (v) { qe := c_valid }
    }
  }

  when (ifuWbValid) {
    ifuWbPtr_write := ifuWbPtr + 1.U
  }

  ftbEntryMem.io.raddr.head := ifuWbIdx
  val hasFalseHit = WireInit(false.B)
  when (RegNext(hitPdValid)) {
    // check for false hit
    val predFtbEntry = ftbEntryMem.io.rdata.head
    // val brSlots = predFtbEntry.brSlots

    // we check cfis that bpu predicted

    // bpu predicted branches but denied by predecode
    val BrFalseHit =
      predFtbEntry.valid && predFtbEntry.sharing &&
        !(pdReg(predFtbEntry.offset).valid && pdReg(predFtbEntry.offset).isBr)

    val jmpOffset = predFtbEntry.offset
    val jmp_pd = pdReg(jmpOffset)
    val jalFalseHit = predFtbEntry.jmpValid &&
      ((predFtbEntry.isJal  && !(jmp_pd.valid && jmp_pd.isJal)) ||
       (predFtbEntry.isJalr && !(jmp_pd.valid && jmp_pd.isJalr)) ||
       (predFtbEntry.isCall && !(jmp_pd.valid && jmp_pd.isCall)) ||
       (predFtbEntry.isRet  && !(jmp_pd.valid && jmp_pd.isRet))
      )

    hasFalseHit := BrFalseHit || jalFalseHit || hitPdMispredReg
    XSDebug(hasFalseHit, "FTB false hit by br or jal or hit_pd, startAddr: %x\n", pdWb.bits.pc(0))

    // assert(!hasFalseHit)
  }

  when (hasFalseHit) {
    entryHitStatus(wbIdxReg) := h_false_hit
  }

  /** FTQ to Backend
   *  Since Backend has a pc memory copy, it write this memory at the same time.
   */
  // to backend pc mem / target
  io.toBackend.pc_mem_wen   := RegNext(lastCycleBpuIn)
  io.toBackend.pc_mem_waddr := RegEnable(lastCycleBpuInIdx, lastCycleBpuIn)
  io.toBackend.pc_mem_wdata := RegEnable(bpuInBypass, lastCycleBpuIn)

  /** Redirect from backend
   */

  // redirect read cfiInfo, couples to redirectGen s2
  ftqRedirectSram.io.ren.init.last := backendRedirect.valid
  ftqRedirectSram.io.raddr.init.last := backendRedirect.bits.ftqIdx.value

  ftbEntryMem.io.raddr.init.last := backendRedirect.bits.ftqIdx.value

  val stage3CfiInfo = ftqRedirectSram.io.rdata.init.last
  val fromBackendRedirect = WireInit(backendRedirectReg)
  val backendRedirectCfi = fromBackendRedirect.bits.cfiUpdate
  backendRedirectCfi.fromFtqRedirectSram(stage3CfiInfo)

  val r_ftb_entry = ftbEntryMem.io.rdata.init.last
  val r_ftqOffset = fromBackendRedirect.bits.ftqOffset

  when (entryHitStatus(fromBackendRedirect.bits.ftqIdx.value) === h_hit) {
    backendRedirectCfi.shift := r_ftb_entry.getBrMaskByOffset(r_ftqOffset) +&
      (backendRedirectCfi.pd.isBr && !r_ftb_entry.brIsRecorded(r_ftqOffset) &&
      !r_ftb_entry.newBrCanNotInsert(r_ftqOffset))

    backendRedirectCfi.addIntoHist := backendRedirectCfi.pd.isBr && (r_ftb_entry.brIsRecorded(r_ftqOffset) ||
        !r_ftb_entry.newBrCanNotInsert(r_ftqOffset))
  }.otherwise {
    backendRedirectCfi.shift := (backendRedirectCfi.pd.isBr && backendRedirectCfi.taken).asUInt
    backendRedirectCfi.addIntoHist := backendRedirectCfi.pd.isBr.asUInt
  }

  /** Redirect from ifu
   */
  val fromIfuRedirect = WireInit(0.U.asTypeOf(Valid(new Redirect)))
  fromIfuRedirect.valid := pdWb.valid && pdWb.bits.misOffset.valid && !backendFlush
  fromIfuRedirect.bits.ftqIdx := pdWb.bits.ftqIdx
  fromIfuRedirect.bits.ftqOffset := pdWb.bits.misOffset.bits
  fromIfuRedirect.bits.level := RedirectLevel.flushAfter

  val ifuRedirectCfiUpdate = fromIfuRedirect.bits.cfiUpdate
  ifuRedirectCfiUpdate.pc := pdWb.bits.pc(pdWb.bits.misOffset.bits)
  ifuRedirectCfiUpdate.pd := pdWb.bits.pd(pdWb.bits.misOffset.bits)
  ifuRedirectCfiUpdate.predTaken := cfiIndexVec(pdWb.bits.ftqIdx.value).valid
  ifuRedirectCfiUpdate.target := pdWb.bits.target
  ifuRedirectCfiUpdate.taken := pdWb.bits.cfiOffset.valid
  ifuRedirectCfiUpdate.isMisPred := pdWb.bits.misOffset.valid

  val ifuRedirectReg = RegNext(fromIfuRedirect, init=0.U.asTypeOf(Valid(new Redirect)))
  val ifuRedirectToBpu = WireInit(ifuRedirectReg)
  ifuFlush := fromIfuRedirect.valid || ifuRedirectToBpu.valid

  ftqRedirectSram.io.ren.head := fromIfuRedirect.valid
  ftqRedirectSram.io.raddr.head := fromIfuRedirect.bits.ftqIdx.value

  ftbEntryMem.io.raddr.head := fromIfuRedirect.bits.ftqIdx.value

  val toBpuCfi = ifuRedirectToBpu.bits.cfiUpdate
  toBpuCfi.fromFtqRedirectSram(ftqRedirectSram.io.rdata.head)
  when (ifuRedirectReg.bits.cfiUpdate.pd.isRet) {
    toBpuCfi.target := toBpuCfi.rasEntry.retAddr
  }

  /** Writeback from exu
   * a part of logic from backend redirect.
   */
  backendRedirect := io.fromBackend.redirect

  def extractRedirectInfo(wb: Valid[Redirect]) = {
    val ftqPtr = wb.bits.ftqIdx
    val ftqOffset = wb.bits.ftqOffset
    val taken = wb.bits.cfiUpdate.taken
    val mispred = wb.bits.cfiUpdate.isMisPred
    (wb.valid, ftqPtr, ftqOffset, taken, mispred)
  }

  // fix mispredict entry
  val lastIsMispredict = RegNext(
    backendRedirect.valid && backendRedirect.bits.level === RedirectLevel.flushAfter, init = false.B
  )

  def updateCfiInfo(redirect: Valid[Redirect], isBackend: Boolean = true) = {
    val (r_valid, r_ptr, r_offset, r_taken, r_mispred) = extractRedirectInfo(redirect)
    val r_idx = r_ptr.value
    val cfiIndex_bits_wen = r_valid && r_taken && r_offset < cfiIndexVec(r_idx).bits
    val cfiIndex_valid_wen = r_valid && r_offset === cfiIndexVec(r_idx).bits
    when (cfiIndex_bits_wen || cfiIndex_valid_wen) {
      cfiIndexVec(r_idx).valid := cfiIndex_bits_wen || cfiIndex_valid_wen && r_taken
    }
    when (cfiIndex_bits_wen) {
      cfiIndexVec(r_idx).bits := r_offset
    }
    newestEntryTarget := redirect.bits.cfiUpdate.target
    newestEntryPtr := r_ptr
    updateTarget(r_idx) := redirect.bits.cfiUpdate.target // TODO: remove this
    if (isBackend) {
      mispredictVec(r_idx)(r_offset) := r_mispred
    }
  }

  when(backendRedirectReg.valid) {
    updateCfiInfo(backendRedirectReg)
  }.elsewhen (ifuRedirectToBpu.valid) {
    updateCfiInfo(ifuRedirectToBpu, isBackend=false)
  }

  /** Flush ptr and state queue
   * A part of logic from reirect.
   */
  val redirectVec = VecInit(backendRedirect, fromIfuRedirect)

  // when redirect, we should reset ptrs and status queues
  when(redirectVec.map(r => r.valid).reduce(_||_)){
    val r = PriorityMux(redirectVec.map(r => (r.valid -> r.bits)))
    val notIfu = redirectVec.dropRight(1).map(r => r.valid).reduce(_||_)
    val (idx, offset, flushItSelf) = (r.ftqIdx, r.ftqOffset, RedirectLevel.flushItself(r.level))
    val next = idx + 1.U
    bpuPtr := next
    io.toIbuffer.bits := next
    io.toIbuffer.valid := true.B
    copied_bpu_ptr.foreach(_ := next)
    ifuPtr_write := next
    ifuWbPtr_write := next
    ifuPtrPlus1_write := idx + 2.U
    ifuPtrPlus2_write := idx + 3.U
    when (notIfu) {
      commitStateQueue(idx.value).zipWithIndex.foreach({ case (s, i) =>
        when(i.U > offset || i.U === offset && flushItSelf){
          s := c_invalid
        }
      })
    }
  }

  // only the valid bit is actually needed
  io.toIfu.redirect.bits    := backendRedirect.bits
  io.toIfu.redirect.valid   := stage2Flush

  // commit
  for (c <- io.fromBackend.rob_commits) {
    when(c.valid) {
      commitStateQueue(c.bits.ftqIdx.value)(c.bits.ftqOffset) := c_commited
      // TODO: remove this
      // For instruction fusions, we also update the next instruction
      when (c.bits.commitType === 4.U) {
        commitStateQueue(c.bits.ftqIdx.value)(c.bits.ftqOffset + 1.U) := c_commited
      }.elsewhen(c.bits.commitType === 5.U) {
        commitStateQueue(c.bits.ftqIdx.value)(c.bits.ftqOffset + 2.U) := c_commited
      }.elsewhen(c.bits.commitType === 6.U) {
        val index = (c.bits.ftqIdx + 1.U).value
        commitStateQueue(index)(0) := c_commited
      }.elsewhen(c.bits.commitType === 7.U) {
        val index = (c.bits.ftqIdx + 1.U).value
        commitStateQueue(index)(1) := c_commited
      }
    }
  }

  /** FTQ TO BPU
   * Redirect to BPU.
   * Commit update to BPU.
   */

  io.toBpu.redirect := Mux(fromBackendRedirect.valid, fromBackendRedirect, ifuRedirectToBpu)

  val mayHaveStallFromBpu = Wire(Bool())
  val BpuFtbUpdateStall = RegInit(0.U(2.W)) // 2-cycle stall, so we need 3 states
  mayHaveStallFromBpu := BpuFtbUpdateStall =/= 0.U

  canCommit := commPtr =/= ifuWbPtr && !mayHaveStallFromBpu &&
    Cat(commitStateQueue(commPtr.value).map(s => {
      s === c_invalid || s === c_commited
    })).andR

  val mmioReadPtr = io.mmioCommitRead.mmioFtqPtr
  val mmioLastCommit = isBefore(commPtr, mmioReadPtr) && (isAfter(ifuPtr,mmioReadPtr)  ||  mmioReadPtr ===   ifuPtr) &&
                       Cat(commitStateQueue(mmioReadPtr.value).map(s => { s === c_invalid || s === c_commited})).andR
  io.mmioCommitRead.mmioLastCommit := RegNext(mmioLastCommit)

  // commit reads
  val commitPcBundle = RegNext(ftqPcMem.io.commPtr_rdata)
  val commitTarget =
    Mux(RegNext(commPtr === newestEntryPtr),
      RegNext(newestEntryTarget),
      RegNext(ftqPcMem.io.commPtrPlus1_rdata.startAddr))
  ftqPdMem.io.raddr.last := commPtr.value
  val commitPd = ftqPdMem.io.rdata.last
  ftqRedirectSram.io.ren.last := canCommit
  ftqRedirectSram.io.raddr.last := commPtr.value
  val commitSpecInfo = ftqRedirectSram.io.rdata.last
  ftqMetaSram.io.ren(0) := canCommit
  ftqMetaSram.io.raddr(0) := commPtr.value
  val commitMeta = ftqMetaSram.io.rdata(0)
  ftbEntryMem.io.raddr.last := commPtr.value
  val commitFtbEntry = ftbEntryMem.io.rdata.last

  // need one cycle to read mem and srams
  val doCommitPtr = RegNext(commPtr)
  val doCommit = RegNext(canCommit, init=false.B)
  when (canCommit) {
    commPtr_write := commPtrPlus1
    commPtrPlus1_write := commPtrPlus1 + 1.U
  }
  val commitStatus = RegNext(commitStateQueue(commPtr.value))
  val canCommitCfi = WireInit(cfiIndexVec(commPtr.value))
  when (commitStateQueue(commPtr.value)(canCommitCfi.bits) =/= c_commited) {
    canCommitCfi.valid := false.B
  }
  val commitCfi = RegNext(canCommitCfi)

  val commitMispredict = VecInit((RegNext(mispredictVec(commPtr.value)) zip commitStatus).map {
    case (mis, state) => mis && state === c_commited
  })
  val canCommitHit = entryHitStatus(commPtr.value)
  val commitHit = RegNext(canCommitHit)
  val diff_commit_target = RegNext(updateTarget(commPtr.value)) // TODO: remove this
  val commitStage = RegNext(predStage(commPtr.value))
  val commitValid = commitHit === h_hit || commitCfi.valid // hit or taken

  val toBpuHit = canCommitHit === h_hit || canCommitHit === h_false_hit
  switch (BpuFtbUpdateStall) {
    is (0.U) {
      when (canCommitCfi.valid && !toBpuHit && canCommit) {
        BpuFtbUpdateStall := 2.U // 2-cycle stall
      }
    }
    is (2.U) {
      BpuFtbUpdateStall := 1.U
    }
    is (1.U) {
      BpuFtbUpdateStall := 0.U
    }
    is (3.U) {
      XSError(true.B, "BpuFtbUpdateStall should be 0, 1 or 2")
    }
  }

  // TODO: remove this
  XSError(doCommit && diff_commit_target =/= commitTarget, "\ncommit target should be the same as update target\n")

  io.toBpu.update := DontCare
  io.toBpu.update.valid := commitValid && doCommit
  val update = io.toBpu.update.bits
  update.falseHit   := commitHit === h_false_hit
  update.pc          := commitPcBundle.startAddr
  update.meta        := commitMeta.meta
  update.cfi_idx     := commitCfi
  update.fullTarget := commitTarget
  update.fromStage  := commitStage
  update.specInfo   := commitSpecInfo

  val commit_real_hit = commitHit === h_hit
  val update_ftb_entry = update.ftbEntry

  val ftbEntryGen = Module(new FTBEntryGen).io
  ftbEntryGen.start_addr     := commitPcBundle.startAddr
  ftbEntryGen.oldEntry       := commitFtbEntry
  ftbEntryGen.pd             := commitPd
  ftbEntryGen.cfiIndex       := commitCfi
  ftbEntryGen.target         := commitTarget
  ftbEntryGen.hit            := commit_real_hit
  ftbEntryGen.mispredictVec := commitMispredict

  update_ftb_entry         := ftbEntryGen.new_entry
  update.new_br_insert_pos := ftbEntryGen.new_br_insert_pos
  update.mispred_mask      := ftbEntryGen.mispred_mask
  update.oldEntry         := ftbEntryGen.is_old_entry
  update.predHit          := commitHit === h_hit || commitHit === h_false_hit
  update.br_taken     := ftbEntryGen.taken_mask
  update.jmp_taken         := ftbEntryGen.jmp_taken

  // ****************************************************************
  // *********************** to prefetch ****************************
  // ****************************************************************

  ftqPcMem.io.other_raddrs(0) := DontCare
  if(cacheParams.hasPrefetch){
    val prefetchPtr = RegInit(FtqPtr(false.B, 0.U))
    val diff_prefetch_addr = WireInit(updateTarget(prefetchPtr.value)) //TODO: remove this

    prefetchPtr := prefetchPtr + io.toPrefetch.req.fire

    ftqPcMem.io.other_raddrs(0) := prefetchPtr.value

    when (bpuS2Redirect && !isBefore(prefetchPtr, bpus2Resp.ftqIdx)) {
      prefetchPtr := bpus2Resp.ftqIdx
    }

    when (bpuS3Redirect && !isBefore(prefetchPtr, bpuS3Resp.ftqIdx)) {
      prefetchPtr := bpuS3Resp.ftqIdx
      // XSError(true.B, "\ns3_redirect mechanism not implemented!\n")
    }


    val prefetch_is_to_send = WireInit(entryFetchStatus(prefetchPtr.value) === f_to_send)
    val prefetch_addr = Wire(UInt(VAddrBits.W))

    when (lastCycleBpuIn && bpuInBypassPtr === prefetchPtr) {
      prefetch_is_to_send := true.B
      prefetch_addr := lastCycleBpuTarget
      diff_prefetch_addr := lastCycleBpuTarget // TODO: remove this
    }.otherwise{
      prefetch_addr := RegNext( ftqPcMem.io.other_rdatas(0).startAddr)
    }
    io.toPrefetch.req.valid := prefetchPtr =/= bpuPtr && prefetch_is_to_send
    io.toPrefetch.req.bits.target := prefetch_addr

    when(redirectVec.map(r => r.valid).reduce(_||_)){
      val r = PriorityMux(redirectVec.map(r => (r.valid -> r.bits)))
      val next = r.ftqIdx + 1.U
      prefetchPtr := next
    }

    // TODO: remove this
    // XSError(io.toPrefetch.req.valid && diff_prefetch_addr =/= prefetch_addr,
    //         f"\nprefetch_req_target wrong! prefetchPtr: ${prefetchPtr}, prefetch_addr: ${Hexadecimal(prefetch_addr)} diff_prefetch_addr: ${Hexadecimal(diff_prefetch_addr)}\n")


    XSError(isBefore(bpuPtr, prefetchPtr) && !isFull(bpuPtr, prefetchPtr), "\nprefetchPtr is before bpuPtr!\n")
    XSError(isBefore(prefetchPtr, ifuPtr) && !isFull(ifuPtr, prefetchPtr), "\nifuPtr is before prefetchPtr!\n")
  }
  else {
    io.toPrefetch.req <> DontCare
  }

  // ******************************************************************************
  // **************************** commit perf counters ****************************
  // ******************************************************************************

  val commit_inst_mask    = VecInit(commitStatus.map(c => c === c_commited && doCommit)).asUInt
  val commit_mispred_mask = commitMispredict.asUInt
  val commit_not_mispred_mask = (~commit_mispred_mask).asUInt

  val commit_br_mask = commitPd.brMask.asUInt
  val commit_jmp_mask = UIntToOH(commitPd.jmpOffset) & Fill(PredictWidth, commitPd.jmpInfo.valid.asTypeOf(UInt(1.W)))
  val commit_cfi_mask = (commit_br_mask | commit_jmp_mask)

  val mbpInstrs = commit_inst_mask & commit_cfi_mask

  val mbpRights = mbpInstrs & commit_not_mispred_mask
  val mbpWrongs = mbpInstrs & commit_mispred_mask

  io.bpuInfo.bpRight := PopCount(mbpRights)
  io.bpuInfo.bpWrong := PopCount(mbpWrongs)

  // Cfi Info
  for (i <- 0 until PredictWidth) {
    val pc = commitPcBundle.startAddr + (i * instBytes).U
    val v = commitStatus(i) === c_commited
    val isBr = commitPd.brMask(i)
    val isJmp = commitPd.jmpInfo.valid && commitPd.jmpOffset === i.U
    val isCfi = isBr || isJmp
    val isTaken = commitCfi.valid && commitCfi.bits === i.U
    val misPred = commitMispredict(i)
    // val ghist = commitSpecInfo.ghist.predHist
    val histPtr = commitSpecInfo.histPtr
    val predCycle = commitMeta.meta(63, 0)
    val target = commitTarget

    val brIdx = OHToUInt(Reverse(update_ftb_entry.brValid && update_ftb_entry.offset === i.U))
    val inFtbEntry = update_ftb_entry.brValid && update_ftb_entry.offset === i.U
    val addIntoHist = ((commitHit === h_hit) && inFtbEntry) || ((!(commitHit === h_hit) && i.U === commitCfi.bits && isBr && commitCfi.valid))
    XSDebug(v && doCommit && isCfi, p"cfi_update: isBr(${isBr}) pc(${Hexadecimal(pc)}) " +
    p"taken(${isTaken}) mispred(${misPred}) cycle($predCycle) hist(${histPtr.value}) " +
    p"startAddr(${Hexadecimal(commitPcBundle.startAddr)}) AddIntoHist(${addIntoHist}) " +
    p"brInEntry(${inFtbEntry}) brIdx(${brIdx}) target(${Hexadecimal(target)})\n")
  }

  val enq = io.fromBpu.resp
  val perf_redirect = backendRedirect

  XSPerfAccumulate("entry", validEntries)
  XSPerfAccumulate("bpu_to_ftq_stall", enq.valid && !enq.ready)
  XSPerfAccumulate("mispredictRedirect", perf_redirect.valid && RedirectLevel.flushAfter === perf_redirect.bits.level)
  XSPerfAccumulate("replayRedirect", perf_redirect.valid && RedirectLevel.flushItself(perf_redirect.bits.level))
  XSPerfAccumulate("predecodeRedirect", fromIfuRedirect.valid)

  XSPerfAccumulate("to_ifu_bubble", io.toIfu.req.ready && !io.toIfu.req.valid)

  XSPerfAccumulate("to_ifu_stall", io.toIfu.req.valid && !io.toIfu.req.ready)
  XSPerfAccumulate("from_bpu_real_bubble", !enq.valid && enq.ready && allowBpuIn)
  XSPerfAccumulate("bpu_to_ifu_bubble", bpuPtr === ifuPtr)

  val from_bpu = io.fromBpu.resp.bits
  def in_entry_len_map_gen(resp: BpuToFtqBundle)(stage: String) = {
    val entry_len = (resp.lastStageFtbEntry.getFallThrough(resp.s3.pc(dupForFtq)) - resp.s3.pc(dupForFtq)) >> instOffsetBits
    val entry_len_recording_vec = (1 to PredictWidth+1).map(i => entry_len === i.U)
    val entry_len_map = (1 to PredictWidth+1).map(i =>
      f"${stage}_ftb_entry_len_$i" -> (entry_len_recording_vec(i-1) && resp.s3.valid(dupForFtq))
    ).foldLeft(Map[String, UInt]())(_+_)
    entry_len_map
  }
  val s3_entry_len_map = in_entry_len_map_gen(from_bpu)("s3")

  val to_ifu = io.toIfu.req.bits



  val commit_num_inst_recording_vec = (1 to PredictWidth).map(i => PopCount(commit_inst_mask) === i.U)
  val commit_num_inst_map = (1 to PredictWidth).map(i =>
    f"commit_num_inst_$i" -> (commit_num_inst_recording_vec(i-1) && doCommit)
  ).foldLeft(Map[String, UInt]())(_+_)



  val commit_jal_mask  = UIntToOH(commitPd.jmpOffset) & Fill(PredictWidth, commitPd.hasJal.asTypeOf(UInt(1.W)))
  val commit_jalr_mask = UIntToOH(commitPd.jmpOffset) & Fill(PredictWidth, commitPd.hasJalr.asTypeOf(UInt(1.W)))
  val commit_call_mask = UIntToOH(commitPd.jmpOffset) & Fill(PredictWidth, commitPd.hasCall.asTypeOf(UInt(1.W)))
  val commit_ret_mask  = UIntToOH(commitPd.jmpOffset) & Fill(PredictWidth, commitPd.hasRet.asTypeOf(UInt(1.W)))


  val mbpBRights = mbpRights & commit_br_mask
  val mbpJRights = mbpRights & commit_jal_mask
  val mbpIRights = mbpRights & commit_jalr_mask
  val mbpCRights = mbpRights & commit_call_mask
  val mbpRRights = mbpRights & commit_ret_mask

  val mbpBWrongs = mbpWrongs & commit_br_mask
  val mbpJWrongs = mbpWrongs & commit_jal_mask
  val mbpIWrongs = mbpWrongs & commit_jalr_mask
  val mbpCWrongs = mbpWrongs & commit_call_mask
  val mbpRWrongs = mbpWrongs & commit_ret_mask

  val commit_pred_stage = RegNext(predStage(commPtr.value))

  def pred_stage_map(src: UInt, name: String) = {
    (0 until numBpStages).map(i =>
      f"${name}_stage_${i+1}" -> PopCount(src.asBools.map(_ && commit_pred_stage === BP_STAGES(i)))
    ).foldLeft(Map[String, UInt]())(_+_)
  }

  val mispred_stage_map      = pred_stage_map(mbpWrongs,  "mispredict")
  val br_mispred_stage_map   = pred_stage_map(mbpBWrongs, "br_mispredict")
  val jalr_mispred_stage_map = pred_stage_map(mbpIWrongs, "jalr_mispredict")
  val correct_stage_map      = pred_stage_map(mbpRights,  "correct")
  val br_correct_stage_map   = pred_stage_map(mbpBRights, "br_correct")
  val jalr_correct_stage_map = pred_stage_map(mbpIRights, "jalr_correct")

  val update_valid = io.toBpu.update.valid
  def u(cond: Bool) = update_valid && cond
  val ftb_false_hit = u(update.falseHit)
  // assert(!ftb_false_hit)
  val ftb_hit = u(commitHit === h_hit)

  val ftb_new_entry = u(ftbEntryGen.is_init_entry)
  val ftb_new_entry_only_br = ftb_new_entry && !update_ftb_entry.jmpValid
  val ftb_new_entry_only_jmp = ftb_new_entry && !update_ftb_entry.brValid(0)
  val ftb_new_entry_has_br_and_jmp = ftb_new_entry && update_ftb_entry.brValid(0) && update_ftb_entry.jmpValid

  val ftb_old_entry = u(ftbEntryGen.is_old_entry)

  val ftb_modified_entry = u(ftbEntryGen.is_new_br || ftbEntryGen.is_jalr_target_modified || ftbEntryGen.is_always_taken_modified)
  val ftb_modified_entry_new_br = u(ftbEntryGen.is_new_br)
  val ftb_modified_entry_jalr_target_modified = u(ftbEntryGen.is_jalr_target_modified)
  val ftb_modified_entry_br_full = ftb_modified_entry && ftbEntryGen.is_br_full
  val ftb_modified_entry_always_taken = ftb_modified_entry && ftbEntryGen.is_always_taken_modified

  val ftb_entry_len = (ftbEntryGen.new_entry.getFallThrough(update.pc) - update.pc) >> instOffsetBits
  val ftb_entry_len_recording_vec = (1 to PredictWidth+1).map(i => ftb_entry_len === i.U)
  val ftb_init_entry_len_map = (1 to PredictWidth+1).map(i =>
    f"ftb_init_entry_len_$i" -> (ftb_entry_len_recording_vec(i-1) && ftb_new_entry)
  ).foldLeft(Map[String, UInt]())(_+_)
  val ftb_modified_entry_len_map = (1 to PredictWidth+1).map(i =>
    f"ftb_modified_entry_len_$i" -> (ftb_entry_len_recording_vec(i-1) && ftb_modified_entry)
  ).foldLeft(Map[String, UInt]())(_+_)

  val ftq_occupancy_map = (0 to FtqSize).map(i =>
    f"ftq_has_entry_$i" ->( validEntries === i.U)
  ).foldLeft(Map[String, UInt]())(_+_)

  val perfCountsMap = Map(
    "BpInstr" -> PopCount(mbpInstrs),
    "BpBInstr" -> PopCount(mbpBRights | mbpBWrongs),
    "BpRight"  -> PopCount(mbpRights),
    "BpWrong"  -> PopCount(mbpWrongs),
    "BpBRight" -> PopCount(mbpBRights),
    "BpBWrong" -> PopCount(mbpBWrongs),
    "BpJRight" -> PopCount(mbpJRights),
    "BpJWrong" -> PopCount(mbpJWrongs),
    "BpIRight" -> PopCount(mbpIRights),
    "BpIWrong" -> PopCount(mbpIWrongs),
    "BpCRight" -> PopCount(mbpCRights),
    "BpCWrong" -> PopCount(mbpCWrongs),
    "BpRRight" -> PopCount(mbpRRights),
    "BpRWrong" -> PopCount(mbpRWrongs),

    "ftb_false_hit"                -> PopCount(ftb_false_hit),
    "ftb_hit"                      -> PopCount(ftb_hit),
    "ftb_new_entry"                -> PopCount(ftb_new_entry),
    "ftb_new_entry_only_br"        -> PopCount(ftb_new_entry_only_br),
    "ftb_new_entry_only_jmp"       -> PopCount(ftb_new_entry_only_jmp),
    "ftb_new_entry_has_br_and_jmp" -> PopCount(ftb_new_entry_has_br_and_jmp),
    "ftb_old_entry"                -> PopCount(ftb_old_entry),
    "ftb_modified_entry"           -> PopCount(ftb_modified_entry),
    "ftb_modified_entry_new_br"    -> PopCount(ftb_modified_entry_new_br),
    "ftb_jalr_target_modified"     -> PopCount(ftb_modified_entry_jalr_target_modified),
    "ftb_modified_entry_br_full"   -> PopCount(ftb_modified_entry_br_full),
    "ftb_modified_entry_always_taken" -> PopCount(ftb_modified_entry_always_taken)
  ) ++ ftb_init_entry_len_map ++ ftb_modified_entry_len_map ++
  s3_entry_len_map ++ commit_num_inst_map ++ ftq_occupancy_map ++
  mispred_stage_map ++ br_mispred_stage_map ++ jalr_mispred_stage_map ++
  correct_stage_map ++ br_correct_stage_map ++ jalr_correct_stage_map

  for((key, value) <- perfCountsMap) {
    XSPerfAccumulate(key, value)
  }

  // --------------------------- Debug --------------------------------
  // XSDebug(enqFire, p"enq! " + io.fromBpu.resp.bits.toPrintable)
  XSDebug(io.toIfu.req.fire, p"fire to ifu " + io.toIfu.req.bits.toPrintable)
  XSDebug(doCommit, p"deq! [ptr] $doCommitPtr\n")
  XSDebug(true.B, p"[bpuPtr] $bpuPtr, [ifuPtr] $ifuPtr, [ifuWbPtr] $ifuWbPtr [commPtr] $commPtr\n")
  XSDebug(true.B, p"[in] v:${io.fromBpu.resp.valid} r:${io.fromBpu.resp.ready} " +
    p"[out] v:${io.toIfu.req.valid} r:${io.toIfu.req.ready}\n")
  XSDebug(doCommit, p"[deq info] cfiIndex: $commitCfi, $commitPcBundle, target: ${Hexadecimal(commitTarget)}\n")

  val perfEvents = Seq(
    ("bpuS2Redirect        ", bpuS2Redirect                                                             ),
    ("bpuS3Redirect        ", bpuS3Redirect                                                             ),
    ("bpu_to_ftq_stall       ", enq.valid && !enq.ready                                                     ),
    ("mispredictRedirect     ", perf_redirect.valid && RedirectLevel.flushAfter === perf_redirect.bits.level),
    ("replayRedirect         ", perf_redirect.valid && RedirectLevel.flushItself(perf_redirect.bits.level)  ),
    ("predecodeRedirect      ", fromIfuRedirect.valid                                                       ),
    ("to_ifu_bubble          ", io.toIfu.req.ready && !io.toIfu.req.valid                                   ),
    ("from_bpu_real_bubble   ", !enq.valid && enq.ready && allowBpuIn                                       ),
    ("BpInstr                ", PopCount(mbpInstrs)                                                         ),
    ("BpBInstr               ", PopCount(mbpBRights | mbpBWrongs)                                           ),
    ("BpRight                ", PopCount(mbpRights)                                                         ),
    ("BpWrong                ", PopCount(mbpWrongs)                                                         ),
    ("BpBRight               ", PopCount(mbpBRights)                                                        ),
    ("BpBWrong               ", PopCount(mbpBWrongs)                                                        ),
    ("BpJRight               ", PopCount(mbpJRights)                                                        ),
    ("BpJWrong               ", PopCount(mbpJWrongs)                                                        ),
    ("BpIRight               ", PopCount(mbpIRights)                                                        ),
    ("BpIWrong               ", PopCount(mbpIWrongs)                                                        ),
    ("BpCRight               ", PopCount(mbpCRights)                                                        ),
    ("BpCWrong               ", PopCount(mbpCWrongs)                                                        ),
    ("BpRRight               ", PopCount(mbpRRights)                                                        ),
    ("BpRWrong               ", PopCount(mbpRWrongs)                                                        ),
    ("ftb_false_hit          ", PopCount(ftb_false_hit)                                                     ),
    ("ftb_hit                ", PopCount(ftb_hit)                                                           ),
  )
  generatePerfEvent()
}