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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xs.utils._
import xiangshan.ExceptionNO._

class IbufPtr(implicit p: Parameters) extends CircularQueuePtr[IbufPtr](
  p => p(XSCoreParamsKey).IBufSize
){
}

class IBufferIO(implicit p: Parameters) extends XSBundle {
  val flush = Input(Bool())
  val in = Flipped(DecoupledIO(new FetchToIBuffer))
  val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val full = Output(Bool())
  val fromFtq = Input(new FtqPtr)
}

class IBufEntry(implicit p: Parameters) extends XSBundle {
  val inst = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  val foldpc = UInt(MemPredPCWidth.W)
  val pd = new PreDecodeInfo
  val pred_taken = Bool()
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(log2Ceil(PredictWidth).W)
  val ipf = Bool()
  val acf = Bool()
  val crossPageIPFFix = Bool()
  val triggered = new TriggerCf
  val mmioFetch = Bool()

  def fromFetch(fetch: FetchToIBuffer, i: Int): IBufEntry = {
    inst   := fetch.instrs(i)
    pc     := fetch.pc(i)
    foldpc := fetch.foldpc(i)
    pd     := fetch.pd(i)
    pred_taken := fetch.ftqOffset(i).valid
    ftqPtr := fetch.ftqPtr
    ftqOffset := fetch.ftqOffset(i).bits
    ipf := fetch.ipf(i)
    acf := fetch.acf(i)
    crossPageIPFFix := fetch.crossPageIPFFix(i)
    triggered := fetch.triggered(i)
    mmioFetch := fetch.mmioFetch
    this
  }

  def toCtrlFlow: CtrlFlow = {
    val cf = Wire(new CtrlFlow)
    cf.instr := inst
    cf.pc := pc
    cf.foldpc := foldpc
    cf.exceptionVec := 0.U.asTypeOf(ExceptionVec())
    cf.exceptionVec(instrPageFault) := ipf
    cf.exceptionVec(instrAccessFault) := acf
    cf.trigger := triggered
    cf.pd := pd
    cf.pred_taken := pred_taken
    cf.crossPageIPFFix := crossPageIPFFix
    cf.storeSetHit := DontCare
    cf.waitForRobIdx := DontCare
    cf.loadWaitBit := DontCare
    cf.loadWaitStrict := DontCare
    cf.ssid := DontCare
    cf.ftqPtr := ftqPtr
    cf.ftqOffset := ftqOffset
    cf
  }
}

class IBufQueue(entryNum:Int, enqNum:Int, deqNum:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val w = Input(Vec(enqNum, new Bundle{
      val en: Bool = Bool()
      val addr: UInt = UInt(log2Ceil(entryNum).W)
      val data: IBufEntry = new IBufEntry
    }))
    val r = Vec(deqNum, new Bundle{
      val addr: UInt = Input(UInt(log2Ceil(entryNum).W))
      val data: IBufEntry = Output(new IBufEntry)
    })
  })
  private val array = Reg(Vec(entryNum, new IBufEntry))
  for ((mem, i) <- array.zipWithIndex) {
    val valids = io.w.map(wreq => wreq.en && wreq.addr === i.U)
    val wdata = io.w.map(_.data)
    val data = Mux1H(valids, wdata)
    when(valids.reduce(_ | _)) {
      mem := data
    }
  }
  for(r <- io.r){
    val rSel = array.indices.map(_.U === r.addr)
    r.data := Mux1H(rSel, array)
  }
}

class ibufDeqDriver(deqNum:Int)(implicit p: Parameters)extends XSModule {
  val io = IO(new Bundle{
    val in = Input(Vec(deqNum, Valid(new CtrlFlow)))
    val deq = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
    val flush = Input(Bool())
  })
  val validsReg = RegInit(VecInit(Seq.fill(deqNum)(false.B)))
  val bitsRegs = Reg(Vec(deqNum, new CtrlFlow))
  io.deq.zip(validsReg).zip(bitsRegs).foreach({case((d, v), b) =>
    d.valid := v && !io.flush
    d.bits := b
  })
  for(((in, v), b) <- io.in.zip(validsReg).zip(bitsRegs)){
    when(io.flush){
      v := false.B
    }.otherwise{
      v := in.valid
    }
    when(in.valid){
      b := in.bits
    }
  }
}

class Ibuffer(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper with HasPerfEvents {
  val io = IO(new IBufferIO)

  val ibuf = Module(new IBufQueue(IBufSize, PredictWidth, DecodeWidth))
  val deqDriver = Module(new ibufDeqDriver(DecodeWidth))
  deqDriver.io.flush := io.flush
  val deqPtrVec = RegInit(VecInit.tabulate(DecodeWidth)(_.U.asTypeOf(new IbufPtr)))
  val deqPtrVecNext =  deqPtrVec.map(WireInit(_))
  val deqPtr = deqPtrVec.head
  val enqPtr = RegInit(0.U.asTypeOf(new IbufPtr))
  val enqPtrDup = RegInit(0.U.asTypeOf(new IbufPtr))

  val validEntries = distanceBetween(enqPtr, deqPtr)
  val allowEnq = RegInit(true.B)

  val numEnq = Mux(io.in.fire, PopCount(io.in.bits.enqEnable), 0.U)
  val numTryDeq = PopCount(io.out.map(_.valid))
  val numDeq = Mux(io.out.head.ready, numTryDeq, 0.U)
  val numAfterEnq = validEntries +& numEnq
  val nextValidEntries = Mux(io.out(0).ready && (numAfterEnq >= numTryDeq), numAfterEnq - numTryDeq, numAfterEnq)
  val nowEmptyEntriesNum = IBufSize.U - nextValidEntries
  allowEnq :=  (nowEmptyEntriesNum >= PredictWidth.U) && !io.flush
  // Enque
  io.in.ready := allowEnq 

  val enqOffset = Seq.tabulate(PredictWidth)(i => PopCount(io.in.bits.valid.asBools.take(i)))
  val enqData = Seq.tabulate(PredictWidth)(i => Wire(new IBufEntry).fromFetch(io.in.bits, i))
  for (i <- 0 until PredictWidth) {
    ibuf.io.w(i).addr := (enqPtrDup + enqOffset(i)).value
    ibuf.io.w(i).data := enqData(i)
    ibuf.io.w(i).en  := io.in.bits.enqEnable(i) && io.in.fire && !io.flush
  }
  when (numEnq =/= 0.U && !io.flush) {
    enqPtr := enqPtr + numEnq
    enqPtrDup := enqPtrDup + numEnq
  }
  ibuf.io.r.take(DecodeWidth).zip(deqPtrVecNext).foreach({case(a, b) =>
    a.addr := b.value
  })
  // Dequeue
  val nextValidVec = Mux(nextValidEntries >= (DecodeWidth + 1).U,
    ((1 << (DecodeWidth + 1)) - 1).U,
    UIntToMask(nextValidEntries(log2Ceil(DecodeWidth + 1) - 1, 0), DecodeWidth + 1)
  )

  for (i <- 0 until DecodeWidth) {
    deqDriver.io.in(i).bits := ibuf.io.r(i).data.toCtrlFlow
    val isJump = deqDriver.io.in(i).bits.pd.valid && (deqDriver.io.in(i).bits.pd.isJal || deqDriver.io.in(i).bits.pd.isJalr)
    val isMMIO = ibuf.io.r(i).data.mmioFetch
    when(isJump) {
      deqDriver.io.in(i).valid := Mux(isMMIO, nextValidVec(i) && (deqDriver.io.in(i).bits.ftqPtr < (io.fromFtq - 1.U)), nextValidVec(i) && nextValidVec(i + 1))
    }.otherwise {
      deqDriver.io.in(i).valid := nextValidVec(i)
    }
    val enqAddrHitsVec = enqOffset.map(d => enqPtr + d).map(_.value === deqPtrVecNext(i).value)
    val enqValidVec = io.in.bits.enqEnable.asBools.map(_ && allowEnq)
    val enqHitsVec = enqAddrHitsVec.zip(enqValidVec).map({case(a, b) => a && b})
    val enqUpdateEn = io.in.fire && Cat(enqHitsVec).orR
    val enqBypassData =  Mux1H(enqHitsVec, enqData)
    when(enqUpdateEn){
      deqDriver.io.in(i).valid := true.B
      deqDriver.io.in(i).bits := enqBypassData.toCtrlFlow
    }
    assert(PopCount(enqHitsVec) <= 1.U)
  }
  deqPtrVecNext.zip(deqPtrVec).foreach({case(dn, d) =>
    dn := d + numDeq
    when(io.out.head.fire) {
      d := dn
    }
  })
  io.out.zip(deqDriver.io.deq).foreach({case(a,b) => a <> b})
  assert(deqPtr <= enqPtrDup)
  io.full := deqPtr.value === enqPtrDup.value && deqPtr.flag =/= enqPtrDup.flag
  when(io.flush){
    allowEnq := true.B
    deqPtrVec := deqPtrVec.indices.map(_.U.asTypeOf(new IbufPtr))
    enqPtr := 0.U.asTypeOf(new IbufPtr)
    enqPtrDup := 0.U.asTypeOf(new IbufPtr)
  }

  // Debug info
  XSDebug(io.flush, "IBuffer Flushed\n")

  when(io.in.fire) {
    XSDebug("Enque:\n")
    XSDebug(p"MASK=${Binary(io.in.bits.valid)}\n")
    for(i <- 0 until PredictWidth){
      XSDebug(p"PC=${Hexadecimal(io.in.bits.pc(i))} ${Hexadecimal(io.in.bits.instrs(i))}\n")
    }
  }

  for (i <- 0 until DecodeWidth) {
    XSDebug(io.out(i).fire,
      p"deq: ${Hexadecimal(io.out(i).bits.instr)} PC=${Hexadecimal(io.out(i).bits.pc)}" +
      p"v=${io.out(i).valid} r=${io.out(i).ready} " +
      p"excpVec=${Binary(io.out(i).bits.exceptionVec.asUInt)} crossPageIPF=${io.out(i).bits.crossPageIPFFix}\n")
  }

  XSDebug(p"ValidEntries: ${validEntries}\n")
  XSDebug(p"EnqNum: ${numEnq}\n")
  XSDebug(p"DeqNum: ${numDeq}\n")

  val afterInit = RegInit(false.B)
  val headBubble = RegInit(false.B)
  when (io.in.fire) { afterInit := true.B }
  when (io.flush) {
    headBubble := true.B
  } .elsewhen(validEntries =/= 0.U) {
    headBubble := false.B
  }
  val instrHungry = afterInit && (validEntries === 0.U) && !headBubble

  QueuePerf(IBufSize, validEntries, !allowEnq)
  XSPerfAccumulate("flush", io.flush)
  XSPerfAccumulate("hungry", instrHungry)
  if (env.EnableTopDown) {
    val ibuffer_IDWidth_hvButNotFull = afterInit && (validEntries =/= 0.U) && (validEntries < DecodeWidth.U) && !headBubble
    XSPerfAccumulate("ibuffer_IDWidth_hvButNotFull", ibuffer_IDWidth_hvButNotFull)
  }

  val perfEvents = Seq(
    ("IBuffer_Flushed  ", io.flush                                                                     ),
    ("IBuffer_hungry   ", instrHungry                                                                  ),
    ("IBuffer_1_4_valid", (validEntries >  (0*(IBufSize/4)).U) & (validEntries < (1*(IBufSize/4)).U)   ),
    ("IBuffer_2_4_valid", (validEntries >= (1*(IBufSize/4)).U) & (validEntries < (2*(IBufSize/4)).U)   ),
    ("IBuffer_3_4_valid", (validEntries >= (2*(IBufSize/4)).U) & (validEntries < (3*(IBufSize/4)).U)   ),
    ("IBuffer_4_4_valid", (validEntries >= (3*(IBufSize/4)).U) & (validEntries < (4*(IBufSize/4)).U)   ),
    ("IBuffer_full     ",  validEntries.andR                                                           ),
    ("Front_Bubble     ", PopCount((0 until DecodeWidth).map(i => io.out(i).ready && !io.out(i).valid)))
  )
  generatePerfEvent()
}
