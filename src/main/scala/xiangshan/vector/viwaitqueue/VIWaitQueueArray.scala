package xiangshan.vector.viwaitqueue

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.ExceptionNO.illegalInstr
import xiangshan.backend.rob.RobPtr
import xiangshan.{FuType, MicroOp, Redirect, SrcType, XSBundle, XSModule}
import xiangshan.vector._
import xiangshan.vector.writeback.VmbPtr
import xiangshan.backend.execute.fu.csr.vcsr._
import xiangshan.vector.EewVal
object WqState {
  def s_updating:UInt = "b0".U
  def s_waiting:UInt = "b1".U
  def apply() = UInt(1.W)
}
class VIWakeQueueEntry(implicit p: Parameters) extends XSBundle{
  val uop = new MicroOp
  val vtypeRdy = Bool()
  val robEnqueued = Bool()
  val mergeIdAlloc = Bool()
  val state = WqState()
}
class ViwqWritePort(implicit p: Parameters) extends XSBundle{
  val wen = Input(Bool())
  val data = Input(new VIWakeQueueEntry)
  val addr = Input(UInt(log2Ceil(VIWaitQueueWidth).W))
}

class ViwqReadPort(implicit p: Parameters) extends XSBundle{
  val addr = Input(UInt(log2Ceil(VIWaitQueueWidth).W))
  val data = new VIWakeQueueEntry
}

class VmsIdAlloc(implicit p: Parameters) extends XSBundle{
  val en = Input(Bool())
  val data = Input(new VmbPtr)
  val addr = Input(UInt(log2Ceil(VIWaitQueueWidth).W))
}

class VIWakeQueueEntryUpdateNetwork(implicit p: Parameters) extends XSModule with HasVectorParameters{
  val io = IO(new Bundle{
    val enq = Input(Valid(new VIWakeQueueEntry))
    val entry = Input(new VIWakeQueueEntry)
    val robEnq = Input(Vec(RenameWidth, Valid(new RobPtr)))
    val vmsResp = Input(Valid(new VmbPtr))
    val vtypeWb = Flipped(ValidIO(new VtypeWbIO))
    val entryNext = Output(new VIWakeQueueEntry)
    val updateEnable = Output(Bool())
  })
  private val entryNext = WireInit(io.entry)
  private val entryEnqNext = io.enq.bits
  entryEnqNext.uop.mergeIdx := DontCare

  private val robEnqHit = io.robEnq.map(r => r.valid && r.bits === io.entry.uop.robIdx).reduce(_|_)
  when(robEnqHit){
    entryNext.robEnqueued := true.B
  }

  when(io.vmsResp.valid){
    entryNext.mergeIdAlloc := true.B
    entryNext.uop.mergeIdx := io.vmsResp.bits
  }

  private val vtypeWbHit = io.vtypeWb.valid && io.vtypeWb.bits.vtypeRegIdx === io.entry.uop.vtypeRegIdx
  when(vtypeWbHit) {
    entryNext.vtypeRdy := true.B
    entryNext.state := WqState.s_updating
    entryNext.uop.vCsrInfo := DontCare
    entryNext.uop.vCsrInfo.vma := io.vtypeWb.bits.vtype(7)
    entryNext.uop.vCsrInfo.vta := io.vtypeWb.bits.vtype(6)
    entryNext.uop.vCsrInfo.vsew := io.vtypeWb.bits.vtype(5, 3)
    entryNext.uop.vCsrInfo.vlmul := io.vtypeWb.bits.vtype(2, 0)
    entryNext.uop.vCsrInfo.vl := io.vtypeWb.bits.vl
    entryNext.uop.vCsrInfo.vlmax := entryNext.uop.vCsrInfo.VLMAXGen()
    entryNext.uop.cf.exceptionVec(illegalInstr) := io.vtypeWb.bits.vtype(8)
  }
  private val vctrlNext = entryNext.uop.vctrl
  private val vctrl = io.entry.uop.vctrl
  private val ctrl = io.entry.uop.ctrl
  private val vcsr = io.entry.uop.vCsrInfo
  private val isWiden = WireInit(vctrl.isWidden && vctrl.eewType(2) =/= EewType.scalar)
  private val isNarrow = vctrl.isNarrow && vctrl.eewType(2) =/= EewType.scalar && vctrl.eew(2) =/= EewVal.mask
  private val isVgei16 = io.entry.uop.ctrl.fuType === FuType.vpermu && vctrl.eewType(0) === EewType.const && vctrl.eew(0) === EewVal.hword
  private val specialLsrc0EncodeSeq = Seq("b01010".U, "b01011".U, "b10000".U, "b10001".U, "b10110".U, "b10111".U)
  private val isSpeicalFp = vctrl.funct6 === "b010010".U && specialLsrc0EncodeSeq.map(_ === ctrl.lsrc(0)).reduce(_ || _)
  private val isFp = vctrl.funct3 === "b101".U || vctrl.funct3 === "b001".U
  private val regularLs = vctrl.isLs && vctrl.eewType(1) === EewType.dc && vctrl.emulType === EmulType.lmul
  private val indexedLs = vctrl.isLs && vctrl.eewType(1) === EewType.const && vctrl.emulType === EmulType.lmul
  private val lmulShift = Wire(UInt(10.W))
  lmulShift := MuxCase(0.U, Seq(
    (vcsr.vlmul === 0.U) -> 8.U,
    (vcsr.vlmul === 1.U) -> 16.U,
    (vcsr.vlmul === 2.U) -> 32.U,
    (vcsr.vlmul === 3.U) -> 64.U,
    (vcsr.vlmul === 5.U) -> 1.U,
    (vcsr.vlmul === 6.U) -> 2.U,
    (vcsr.vlmul === 7.U) -> 4.U,
  ))
  private val rlsEmul = (lmulShift << vctrl.eew(2)(1, 0)) >> vcsr.vsew(1, 0)
  private val ilsEmul = (lmulShift << vctrl.eew(1)(1, 0)) >> vcsr.vsew(1, 0)
  private val vg16vs1Emul = (lmulShift << EewVal.hword) >> vcsr.vsew(1, 0)
  private val isVMVnr = vctrl.funct6 === "b1001111".U && vctrl.funct3 === "b011".U
  private val iiConds = WireInit(VecInit(Seq.fill(13)(false.B)))
  dontTouch(iiConds)

  private val emuls = Seq.fill(3)(Wire(UInt(3.W)))
  for(i <- emuls.indices) {
    when(vctrl.emulType === EmulType.const) {
      emuls(i) := MuxCase(vctrl.emul, Seq(
        (vctrl.eewType(i) === EewType.dc, 0.U),
        (vctrl.eewType(i) === EewType.scalar, 0.U),
        (vctrl.eewType(i) === EewType.const && vctrl.eew(i) === EewVal.mask, 0.U)
      ))
    }.otherwise{
      emuls(i) := MuxCase(vcsr.vlmul, Seq(
        (vctrl.eewType(i) === EewType.dc, 0.U),
        (vctrl.eewType(i) === EewType.scalar, 0.U),
        (vctrl.eewType(i) === EewType.const && vctrl.eew(i) === EewVal.mask, 0.U),
        (vctrl.eewType(i) === EewType.const && vctrl.eew(i) =/= EewVal.mask, (vcsr.vlmul + vctrl.eew(i)) - vcsr.vsew),
        (vctrl.eewType(i) === EewType.sewm2, vcsr.vlmul + 1.U),
        (vctrl.eewType(i) === EewType.sewd2, vcsr.vlmul - 1.U),
        (vctrl.eewType(i) === EewType.sewd4, vcsr.vlmul - 2.U),
        (vctrl.eewType(i) === EewType.sewd8, vcsr.vlmul - 3.U)
      ))
    }
  }

  private val _emuls = Seq.fill(3)(Wire(UInt(3.W)))
  _emuls.zip(emuls).foreach({case(a, b) =>
    a := MuxCase(0.U, Seq(
      (b === 1.U, 1.U),
      (b === 2.U, 3.U),
      (b === 3.U, 7.U),
      (b === 4.U, 5.U) //Illegal
    ))
  })

  private def VGroupIllegal(idx:Int):Bool = {
    require(idx <= 2)
    val vg = if(idx == 2) ctrl.ldest else ctrl.lsrc(idx)
    (vg & _emuls(idx)).orR || _emuls(idx) === 5.U
  }

  private def IllegalOverlapSrc(idx:Int):Bool = {
    require(idx < 2)
    val dst = ctrl.ldest
    val src = ctrl.lsrc(idx)
    val dstEnd = dst + _emuls(2)
    val srcEnd = src + _emuls(idx)
    val overlapLow = Mux(src < dst, dst, src)
    val overlapHigh = Mux(dstEnd < srcEnd, dstEnd, srcEnd)
    val passCond0 = vctrlNext.eewType(2) === EewType.scalar
    val passCond1 = vctrlNext.eew(2) === vctrlNext.eew(idx)
    val passCond2 = (vctrlNext.eew(2).asSInt < vctrlNext.eew(idx).asSInt) && (overlapLow === src)
    val passCond3 = (vctrlNext.eew(2).asSInt > vctrlNext.eew(idx).asSInt) && (overlapHigh === dstEnd)
    val passCond4 = !(ctrl.vdWen && ctrl.srcType(idx) === SrcType.vec)
    val pass = passCond0 || passCond1 || passCond2 || passCond3 || passCond4
    val checkAnyway = vctrl.notOverlay && ctrl.vdWen && ctrl.srcType(idx) === SrcType.vec
    val isOverlap = overlapLow <= overlapHigh
    isOverlap & (!pass || checkAnyway)
  }

  when(io.entry.state === WqState.s_updating) {
    for (((vn, v), et) <- vctrlNext.eew.zip(vctrl.eew).zip(vctrl.eewType)) {
      vn := MuxCase(v, Seq(
        (et === EewType.sew) -> vcsr.vsew,
        (et === EewType.sewm2) -> (vcsr.vsew + 1.U),
        (et === EewType.sewd2) -> (vcsr.vsew - 1.U),
        (et === EewType.sewd4) -> (vcsr.vsew - 2.U),
        (et === EewType.sewd8) -> (vcsr.vsew - 3.U),
      ))
    }
    when(isVgei16) {
      vctrlNext.eewType(0) := MuxCase(EewType.sew, Seq(
        (vcsr.vsew === 0.U) -> EewType.sewm2,
        (vcsr.vsew === 1.U) -> EewType.sew,
        (vcsr.vsew === 2.U) -> EewType.sewd2,
        (vcsr.vsew === 3.U) -> EewType.sewd4,
      ))
      when(vcsr.vsew === 0.U) {
        isWiden := true.B
        vctrlNext.isWidden := true.B
      }
    }

    when(vctrl.emulType === EmulType.const) {
      vctrlNext.emul := vctrl.emul
    }.otherwise{
      when(vctrl.isLs && vctrlNext.eewType(2) === EewType.const){
        vctrlNext.emul := (vcsr.vlmul + vctrl.eew(2)(1, 0)) - vcsr.vsew
      }.otherwise {
        vctrlNext.emul := vcsr.vlmul
      }
    }
    val vregTouchRaw = Wire(UInt(7.W))
    vregTouchRaw := MuxCase(vctrl.nf, Seq(
      (vctrlNext.emul === 1.U(3.W)) -> (vctrl.nf << 1.U),
      (vctrlNext.emul === 2.U(3.W)) -> (vctrl.nf << 2.U),
      (vctrlNext.emul === 3.U(3.W)) -> 8.U,
    ))
    val vregTouch = vregTouchRaw(3, 0)
    when(vctrl.isLs) {
      entryNext.uop.uopNum := MuxCase(0.U, Seq(
        (vctrlNext.eew(2) === 0.U(3.W)) -> (vregTouch << log2Ceil(VLEN / 8)),
        (vctrlNext.eew(2) === 1.U(3.W)) -> (vregTouch << log2Ceil(VLEN / 16)),
        (vctrlNext.eew(2) === 2.U(3.W)) -> (vregTouch << log2Ceil(VLEN / 32)),
        (vctrlNext.eew(2) === 3.U(3.W)) -> (vregTouch << log2Ceil(VLEN / 64)),
      ))
    }.elsewhen(isNarrow || isWiden) {
      entryNext.uop.uopNum := MuxCase(0.U, Seq(
        (vctrlNext.emul === 0.U(3.W)) -> 2.U,
        (vctrlNext.emul === 1.U(3.W)) -> 4.U,
        (vctrlNext.emul === 2.U(3.W)) -> 8.U,
        (vctrlNext.emul === 5.U(3.W)) -> 1.U,
        (vctrlNext.emul === 6.U(3.W)) -> 1.U,
        (vctrlNext.emul === 7.U(3.W)) -> 1.U,
      ))
    }.otherwise {
      entryNext.uop.uopNum := MuxCase(0.U, Seq(
        (vctrlNext.emul === 0.U(3.W)) -> 1.U,
        (vctrlNext.emul === 1.U(3.W)) -> 2.U,
        (vctrlNext.emul === 2.U(3.W)) -> 4.U,
        (vctrlNext.emul === 3.U(3.W)) -> 8.U,
        (vctrlNext.emul === 5.U(3.W)) -> 1.U,
        (vctrlNext.emul === 6.U(3.W)) -> 1.U,
        (vctrlNext.emul === 7.U(3.W)) -> 1.U,
      ))
    }
    when(vctrl.isLs){
      when(vctrl.maskOp){
        entryNext.uop.vCsrInfo.vta := 1.U
        entryNext.uop.vCsrInfo.vl := (io.entry.uop.vCsrInfo.vl +& 7.U) >> 3.U
      }.elsewhen(vctrl.emulType === EmulType.const) {
        entryNext.uop.vCsrInfo.vta := 1.U
        entryNext.uop.vCsrInfo.vl := entryNext.uop.uopNum
      }
    }.elsewhen(isVMVnr) {
      entryNext.uop.vCsrInfo.vl := (1.U << vctrl.emul(1, 0)).asUInt * ((VLEN / 8).U >> vcsr.vsew(1, 0)).asUInt
    }


    iiConds(0) := vcsr.vill
    iiConds(1) := vctrl.vm && ctrl.ldest === 0.U && ctrl.vdWen && !(vctrlNext.eew(2) === EewVal.mask || vctrlNext.eewType(2) === EewType.scalar)
    iiConds(2) := ctrl.fuType === FuType.vfp && isSpeicalFp && (vcsr.vsew === 0.U || vcsr.vsew === 3.U)
    iiConds(3) := ctrl.fuType === FuType.vfp && !isSpeicalFp && (vcsr.vsew === 0.U || vcsr.vsew === 1.U)
    iiConds(4) := (ctrl.fuType === FuType.vdiv || ctrl.fuType === FuType.vpermu) && isFp && (vcsr.vsew === 0.U || vcsr.vsew === 1.U)
    iiConds(5) := (vctrl.isWidden || vctrl.isNarrow) && !vctrl.maskOp && vcsr.vsew === 3.U
    iiConds(6) := (vctrl.isWidden || vctrl.isNarrow) && vctrl.eewType(2) =/= EewType.scalar && vctrl.eew(2) =/= EewVal.mask && vcsr.vlmul === 3.U
    iiConds(7) := (regularLs || indexedLs) && MuxCase(false.B, Seq(
      (vctrlNext.emul === 1.U(3.W)) -> (vctrl.nf > 4.U),
      (vctrlNext.emul === 2.U(3.W)) -> (vctrl.nf > 2.U),
      (vctrlNext.emul === 3.U(3.W)) -> (vctrl.nf > 1.U),
    ))
    iiConds(8) := regularLs && !rlsEmul(6, 0).orR || indexedLs && !ilsEmul(6, 0).orR
    iiConds(9) := Seq.tabulate(2)(i => IllegalOverlapSrc(i)).reduce(_ || _)
    iiConds(10) := Seq.tabulate(3)(i => VGroupIllegal(i)).reduce(_ || _)
    iiConds(11) := isVgei16 && !vg16vs1Emul(6, 0).orR
    iiConds(12) := (vctrl.eewType(1) === EewType.sewd2 || vctrl.eewType(1) === EewType.sewd4 || vctrl.eewType(1) === EewType.sewd8) && MuxCase(false.B, Seq(
      (vctrl.eewType(1) === EewType.sewd2) -> (vcsr.vsew === 0.U),
      (vctrl.eewType(1) === EewType.sewd4) -> (vcsr.vsew <= 1.U),
      (vctrl.eewType(1) === EewType.sewd8) -> (vcsr.vsew =/= 3.U)
    ))

    entryNext.state := WqState.s_waiting
    entryNext.uop.cf.exceptionVec(illegalInstr) := iiConds.asUInt.orR
  }

  io.entryNext := Mux(io.enq.valid, entryEnqNext, entryNext)
  io.updateEnable := io.enq.valid || vtypeWbHit || robEnqHit || io.vmsResp.valid || io.entry.state === WqState.s_updating
}

class VIWaitQueueArray(implicit p: Parameters) extends XSModule with HasVectorParameters{
  private val size = VIWaitQueueWidth
  val io = IO(new Bundle{
    val enq = Vec(VIDecodeWidth, new ViwqWritePort)
    val deq = new ViwqReadPort
    val read = Vec(VIDecodeWidth, new ViwqReadPort)
    val robEnq = Input(Vec(RenameWidth, Valid(new RobPtr)))
    val vmsIdAllocte = Vec(VIDecodeWidth, new VmsIdAlloc)
    val vtypeWb = Flipped(ValidIO(new VtypeWbIO))
    val redirect = Input(Valid(new Redirect))
    val flushMask = Output(UInt(size.W))
  })
  private val array = Reg(Vec(size, new VIWakeQueueEntry))

  io.flushMask := Cat(array.map(e => e.uop.robIdx.needFlush(io.redirect) || !e.robEnqueued).reverse)

  private val updateNetworkSeq = Seq.fill(size)(Module(new VIWakeQueueEntryUpdateNetwork))

  array.zip(updateNetworkSeq).zipWithIndex.foreach({case((a, un), idx) =>
    val enqSel = io.enq.map(e => e.wen && idx.U === e.addr)
    un.io.enq.valid := enqSel.reduce(_|_)
    un.io.enq.bits := Mux1H(enqSel, io.enq.map(_.data))
    un.io.enq.bits.uop.ctrl.fpu := DontCare
    un.io.enq.bits.uop.cf.waitForRobIdx := DontCare
    un.io.enq.bits.uop.cf.trigger := DontCare
    un.io.enq.bits.uop.cf.exceptionVec.foreach(_ := false.B)
    un.io.entry := a
    un.io.robEnq := io.robEnq
    val vmsSel = io.vmsIdAllocte.map(e => e.en && idx.U === e.addr)
    un.io.vmsResp.valid := vmsSel.reduce(_|_)
    un.io.vmsResp.bits := Mux1H(vmsSel, io.vmsIdAllocte.map(_.data))
    un.io.vtypeWb := io.vtypeWb
    when(un.io.updateEnable){
      a := un.io.entryNext
    }
  })

  io.deq.data := array(io.deq.addr)

  io.read.foreach(r => {
    r.data := array(r.addr)
  })
}
