package xiangshan.vector.viwaitqueue
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.vector.{EewType, EewVal}
import xiangshan.{FuOpType, FuType, LSUOpType, MicroOp, Redirect, SrcType, XSModule}
import xs.utils.LogicShiftRight
import xiangshan.FuOpType

class LsSplitUnit(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val uop = Input(new MicroOp)
    val idx = Input(UInt(log2Ceil(VLEN).W))
    val vstart = Input(UInt(7.W))
    val out = Output(new Bundle {
      val shouldRename = Bool()
      val vdAddend = UInt(3.W)
      val vs2Addend = UInt(3.W)
      val fuOpType = FuOpType()
      val prestart = Bool()
      val tail = Bool()
      val segIdx = UInt(log2Ceil(VLEN).W)
    })
  })
  private val store = io.uop.ctrl.fuType === FuType.stu
  private val idx = io.idx
  private val vctrl = io.uop.vctrl
  private val idxSew = vctrl.eew(1)
  private val memSew = vctrl.eew(0)
  private val nf = io.uop.vctrl.nf
  private val segIdx = io.out.segIdx
  private val elmIdx = Wire(UInt(3.W))

  segIdx := MuxCase(idx, Seq(
    (nf === 2.U) -> idx / 2.U,
    (nf === 3.U) -> idx / 3.U,
    (nf === 4.U) -> idx / 4.U,
    (nf === 5.U) -> idx / 5.U,
    (nf === 6.U) -> idx / 6.U,
    (nf === 7.U) -> idx / 7.U,
    (nf === 8.U) -> idx / 8.U,
  ))

  elmIdx := MuxCase(0.U, Seq(
    (nf === 2.U) -> idx % 2.U,
    (nf === 3.U) -> idx % 3.U,
    (nf === 4.U) -> idx % 4.U,
    (nf === 5.U) -> idx % 5.U,
    (nf === 6.U) -> idx % 6.U,
    (nf === 7.U) -> idx % 7.U,
    (nf === 8.U) -> idx % 8.U,
  ))

  private val vlenBytesLen = log2Ceil(VLEN / 8)
  private val canbeRenamed = MuxCase(false.B, Seq(
    (memSew === 0.U) -> (segIdx(vlenBytesLen - 1, 0) === 0.U),
    (memSew === 1.U) -> (segIdx(vlenBytesLen - 2, 0) === 0.U),
    (memSew === 2.U) -> (segIdx(vlenBytesLen - 3, 0) === 0.U),
    (memSew === 3.U) -> (segIdx(vlenBytesLen - 4, 0) === 0.U),
  ))
  io.out.shouldRename := canbeRenamed && !store

  private val regBaseIdx = MuxCase(0.U(segIdx.getWidth.W), Seq(
    (memSew === 0.U) -> LogicShiftRight(segIdx, vlenBytesLen - 0),
    (memSew === 1.U) -> LogicShiftRight(segIdx, vlenBytesLen - 1),
    (memSew === 2.U) -> LogicShiftRight(segIdx, vlenBytesLen - 2),
    (memSew === 3.U) -> LogicShiftRight(segIdx, vlenBytesLen - 3),
  ))
  io.out.vdAddend := MuxCase(elmIdx, Seq(
    (vctrl.emul === 1.U) -> (regBaseIdx + (elmIdx * 2.U)),
    (vctrl.emul === 2.U) -> (regBaseIdx + (elmIdx * 4.U)),
    (vctrl.emul === 3.U) -> (regBaseIdx + (elmIdx * 8.U)),
  ))

  io.out.vs2Addend := MuxCase(0.U, Seq(
    (idxSew === 0.U) -> (idx >> 4.U),
    (idxSew === 1.U) -> (idx >> 3.U),
    (idxSew === 2.U) -> (idx >> 2.U),
    (idxSew === 3.U) -> (idx >> 1.U),
  ))

  private val lFuOpType = Wire(FuOpType())
  private val sFuOpType = Wire(FuOpType())
  lFuOpType := MuxCase(LSUOpType.ld, Seq(
    (memSew === 0.U) -> LSUOpType.lbu,
    (memSew === 1.U) -> LSUOpType.lhu,
    (memSew === 2.U) -> LSUOpType.lwu
  ))
  sFuOpType := MuxCase(LSUOpType.sd, Seq(
    (memSew === 0.U) -> LSUOpType.sb,
    (memSew === 1.U) -> LSUOpType.sh,
    (memSew === 2.U) -> LSUOpType.sw
  ))
  io.out.fuOpType := Mux(store, sFuOpType, lFuOpType)

  io.out.prestart := segIdx < io.vstart
  io.out.tail := segIdx >= io.uop.vCsrInfo.vl
}

class SplitUop(splitNum:Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle{
    val in = Input(Valid(new MicroOp))
    val current = Input(Vec(splitNum, UInt(log2Ceil(VLEN).W)))
    val vstart = Input(UInt(log2Ceil(VLEN).W))
    val out = Output(Vec(splitNum, Valid(new MicroOp)))
  })

  private val lsSplitUnit = Seq.fill(splitNum)(Module(new LsSplitUnit))
  lsSplitUnit.zipWithIndex.foreach({case(m, i) =>
    m.io.uop := io.in.bits
    m.io.vstart := io.vstart
    m.io.idx := io.current(i)
  })

  private val ctrl = io.in.bits.ctrl
  private val vctrl = io.in.bits.vctrl

  private def GenAddend(et:UInt, widenOrNarrow:Bool, idx:UInt):UInt = {
    val res = Wire(UInt(3.W))
    res := MuxCase(0.U, Seq(
      (et === EewType.sew)   -> Mux(widenOrNarrow, LogicShiftRight(idx, 1), idx),
      (et === EewType.sewm2) -> idx, //Only appear in narrow or widen instructions
      (et === EewType.sewd2) -> LogicShiftRight(idx, 1),
      (et === EewType.sewd4) -> LogicShiftRight(idx, 2),
      (et === EewType.sewd8) -> LogicShiftRight(idx, 3),
    ))
    res
  }

  io.out.zipWithIndex.foreach({ case (o, idx) =>
    val currentnum = io.current(idx)
    val lsSu = lsSplitUnit(idx)
    o.valid := io.in.valid && (currentnum < io.in.bits.uopNum)
    o.bits := io.in.bits
    o.bits.uopNum := io.in.bits.uopNum
    o.bits.uopIdx := currentnum
    o.bits.isTail := lsSu.io.out.tail//Only VLS need this
    o.bits.isPrestart := lsSu.io.out.prestart //Only VLS need this
    o.bits.segIdx := lsSu.io.out.segIdx //Only VLS need this

    when(io.in.bits.vctrl.isLs) {
      o.bits.canRename := lsSu.io.out.shouldRename
      o.bits.ctrl.ldest := ctrl.ldest + lsSu.io.out.vdAddend
      o.bits.ctrl.lsrc(0) := ctrl.lsrc(0)
      o.bits.ctrl.lsrc(1) := Mux(ctrl.srcType(1) === SrcType.vec, ctrl.lsrc(1) + lsSu.io.out.vs2Addend, ctrl.lsrc(1))
      o.bits.ctrl.fuOpType := lsSu.io.out.fuOpType
    }.otherwise {
      val onlyOneDest = vctrl.eewType(2) === EewType.dc || vctrl.eewType(2) === EewType.const
      val narrow = vctrl.isNarrow && !vctrl.maskOp
      val narrowOrWiden = narrow | vctrl.isWidden
      val vs1Addend = GenAddend(vctrl.eewType(0), narrowOrWiden, currentnum)
      val vs2Addend = GenAddend(vctrl.eewType(1), narrowOrWiden, currentnum)
      val vdAddend  = GenAddend(vctrl.eewType(2), narrowOrWiden, currentnum)
      when(onlyOneDest) {
        o.bits.canRename := currentnum === 0.U
      }.elsewhen(narrow || (vctrl.isWidden && io.in.bits.ctrl.fuType === FuType.vpermu)) {
        o.bits.canRename := currentnum(0) === 0.U
      }.otherwise {
        o.bits.canRename := true.B
      }
      o.bits.ctrl.ldest := ctrl.ldest + vdAddend
      o.bits.ctrl.lsrc(0) := ctrl.lsrc(0) + vs1Addend
      o.bits.ctrl.lsrc(1) := ctrl.lsrc(1) + vs2Addend
    }
  })
}

object SplitUop {
  def apply(in:Valid[MicroOp], currentNum:Vec[UInt], vstart:UInt, splitNum:Int)(implicit p: Parameters): Vec[Valid[MicroOp]] = {
    val uopSplitter = Module(new SplitUop(splitNum))
    uopSplitter.io.in := in
    uopSplitter.io.current := currentNum
    uopSplitter.io.vstart := vstart
    uopSplitter.io.out
  }
}

class SplitNetwork(splitNum:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(new MicroOp))
    val out = Vec(splitNum, Decoupled(new MicroOp))
    val vstart = Input(UInt(7.W))
    val redirect = Input(Valid(new Redirect))
  })

  private val remain = RegInit(0.U(log2Ceil(VLEN + 1).W))
  private val remainWire = WireInit(remain)
  private val leaving = PopCount(io.out.map(_.fire))
  private val remainUpdate = io.out.map(_.fire).reduce(_|_)
  private val uopNum = io.in.bits.uopNum
  private val currentIndices = RegInit(VecInit(Seq.tabulate(splitNum)(_.U(log2Ceil(VLEN + 1).W))))
  private val currentDefaultVal = VecInit(Seq.tabulate(splitNum)(_.U))
  private val currentWires = WireInit(currentIndices)

  when(io.in.bits.robIdx.needFlush(io.redirect)) {
    remain := 0.U
    currentIndices := currentDefaultVal
  }.elsewhen(remain === 0.U && io.in.valid) {
    remain := uopNum - leaving
    currentIndices.zip(currentDefaultVal).foreach({case(c, rv) => c := rv + leaving})
  }.elsewhen(remainUpdate) {
    remain := remain - leaving
    currentIndices.foreach(c => c := c + leaving)
  }

  when(remain === 0.U && io.in.valid){
    remainWire := uopNum
    currentWires := currentDefaultVal
  }

  io.in.ready := (remainWire === leaving) && !io.redirect.valid

  private val in_v = Wire(Valid(new MicroOp))
  in_v.valid := io.in.valid
  in_v.bits := io.in.bits
  in_v.bits.uopNum := uopNum

  private val out_v = SplitUop(in_v, currentWires, io.vstart, splitNum)

  io.out.zipWithIndex.foreach({ case (o, i) =>
    o.valid := out_v(i).valid
    o.bits := out_v(i).bits
  })

  when(io.in.valid){assert(leaving <= remainWire)}
}
