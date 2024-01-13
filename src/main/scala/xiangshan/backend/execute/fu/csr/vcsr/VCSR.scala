package xiangshan.backend.execute.fu.csr.vcsr

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.vector._
import xiangshan.backend.execute.fu.FuInput
import xiangshan.backend.execute.fu.csr.CSROpType
import xs.utils.{LogicShiftRight, ZeroExt}

class VtypeWbIO(implicit p: Parameters) extends VectorBaseBundle {
    val vtype = UInt(9.W)
    val vl = UInt(8.W)
    val vtypeRegIdx = UInt(log2Ceil(VIVtypeRegsNum).W)
}
class VCSRWithVtypeRenameIO(implicit p: Parameters) extends VectorBaseBundle {
  val vtypeWbToRename = ValidIO(new VtypeWbIO)
  val vtypeRead = new Bundle {
    val readEn = Output(Bool())
    val data = Flipped(ValidIO(UInt(XLEN.W)))
  }
  val vlRead = new Bundle {
    val readEn = Output(Bool())
    val data = Flipped(ValidIO(UInt(XLEN.W)))
  }
  val debug_vtype = Input(UInt(XLEN.W))
  val debug_vl = Input(UInt(XLEN.W))
  val vlUpdate = Input(Valid(UInt(log2Ceil(VLEN + 1).W)))
}

class VCSRWithRobIO(implicit p: Parameters) extends VectorBaseBundle {
  val vstart = Flipped(ValidIO(UInt(XLEN.W)))
  val vxsat  = Flipped(ValidIO(UInt(XLEN.W)))
  val dirty_vs = Input(Bool())
}

class VCsrIO(implicit p: Parameters) extends VectorBaseBundle {
  val vtype = new VCSRWithVtypeRenameIO
  val robWb = new VCSRWithRobIO
  val vstart = Output(UInt(7.W))
  val vcsr    = Output(UInt(3.W))
}

class VtypeStruct(implicit p: Parameters) extends XSBundle {
  val vill      = Bool()
  val reserved  = UInt((XLEN-9).W)
  val vma       = Bool()
  val vta       = Bool()
  val vsew      = UInt(3.W)
  val vlmul     = UInt(3.W)
  require(this.getWidth == XLEN)
}

class VSetFu(implicit p: Parameters) extends XSModule with HasXSParameter {
  val io = IO(new Bundle {
    val in            = Input(Valid(new FuInput(XLEN)))
    val vtypeNew      = Output(UInt(XLEN.W))
    val vlNew         = Output(UInt(XLEN.W))
    val wbToCtrlValid = Output(Bool())
    val vlOld         = Input(UInt(log2Ceil(VLEN + 1).W))
  })
  /** ********************************************************************************************
   * 1.vsetivli
   * 2.vsetvli with src0 === x0 and dest =/= x0
   * 3.need old vl
   * 4.other vsetvl and vsetvli
   *
   * 1: fuOpType === vsetivli
   * 2: fuOpType === vsetvl && imm(19, 11).andR
   * 3: (fuOpType === vsetvli || fuOpType === vsetvl) && imm(19) && !imm(19, 11).andR
   *
   * 1, 2 should bypass to waitqueue; 3, 4 should wait for writeback from csr.
   * ******************************************************************************************* */
  private val imm     = io.in.bits.uop.ctrl.imm
  private val uimm    = imm(4, 0)
  private val zimm_ii = imm(14, 5)
  private val zimm_i  = imm(10, 0)

  private val opType = io.in.bits.uop.ctrl.fuOpType

  private val type1 = opType === CSROpType.vsetivli
  private val type2 = opType === CSROpType.vsetvli && io.in.bits.uop.ctrl.imm(19, 11).andR
  private val type3 = (opType === CSROpType.vsetvl || opType === CSROpType.vsetvli) && imm(19).asBool && !(imm(18, 11).andR)
  private val type4 = (opType === CSROpType.vsetvl || opType === CSROpType.vsetvli) && !imm(19).asBool

  private val avl = Wire(UInt(XLEN.W))

  when(type1){
    avl := ZeroExt(uimm, XLEN)
  }.elsewhen(type3){
    avl := ZeroExt(io.vlOld, XLEN)
  }.otherwise{
    avl := io.in.bits.src(0)
  }

  private def GenVtype(in: UInt, isReg: Boolean):VtypeStruct = {
    val res = Wire(new VtypeStruct)
    if(isReg) {
      res.vill := in(2, 0) === 4.U || in(5).asBool || in(XLEN-1)
    } else {
      res.vill := in(2, 0) === 4.U || in(5).asBool
    }
    res.reserved := 0.U
    res.vma := Mux(res.vill, 0.U, in(7))
    res.vta := Mux(res.vill, 0.U, in(6))
    res.vsew := Mux(res.vill, 0.U, in(5, 3))
    res.vlmul := Mux(res.vill, 0.U, in(2, 0))
    res
  }
  private val iiVtype = GenVtype(zimm_ii, false)
  private val riVtype = GenVtype(zimm_i, false)
  private val rrVtype = GenVtype(io.in.bits.src(1), true)
  private val vtype = MuxCase(0.U.asTypeOf(new VtypeStruct), Seq(
    (opType === CSROpType.vsetivli) -> iiVtype,
    (opType === CSROpType.vsetvli) -> riVtype,
    (opType === CSROpType.vsetvl) -> rrVtype
  ))

  private val vlmul = vtype.vlmul
  private val vsew  = vtype.vsew

  private val vlenBytes = VLEN / 8
  private val vlmax = Wire(UInt(log2Ceil(vlenBytes * 8 + 1).W))
  vlmax := MuxCase(0.U, Seq(
    (vlmul === 0.U) -> ((vlenBytes * 1).U >> vsew),
    (vlmul === 1.U) -> ((vlenBytes * 2).U >> vsew),
    (vlmul === 2.U) -> ((vlenBytes * 4).U >> vsew),
    (vlmul === 3.U) -> ((vlenBytes * 8).U >> vsew),
    (vlmul === 5.U) -> ((vlenBytes / 8).U >> vsew),
    (vlmul === 6.U) -> ((vlenBytes / 4).U >> vsew),
    (vlmul === 7.U) -> ((vlenBytes / 2).U >> vsew)
  ))
  private val vl = Wire(UInt(log2Ceil(VLEN + 1).W))
  when(vtype.vill){
    vl := 0.U
  }.elsewhen(type2 || avl > vlmax){
    vl := vlmax
  }.otherwise{
    vl := avl
  }
  private val wbToCtrlCond = type3 || type4
  io.wbToCtrlValid := io.in.valid && wbToCtrlCond
  io.vtypeNew := Cat(vtype.vill, vtype.vma, vtype.vta, vtype.vsew, vtype.vlmul)
  io.vlNew := vl
}
