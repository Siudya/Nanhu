package xiangshan.backend.execute.fu.csr.vcsr

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import xiangshan._
import xiangshan.vector._
import xiangshan.backend.execute.fu.FuOutput
import xiangshan.backend.rob.RobPtr

class VtypeWbIO(implicit p: Parameters) extends VectorBaseBundle {
    val vtype = UInt(9.W)
    val vl = UInt(8.W)
    val robIdx = new RobPtr
    val vtypeRegIdx = UInt(log2Ceil(VIVtypeRegsNum).W)
    val pdest = UInt(PhyRegIdxWidth.W)
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
}

class VCSRWithRobIO(implicit p: Parameters) extends VectorBaseBundle {
  val vstart = Flipped(ValidIO(UInt(XLEN.W)))
  val vxsat  = Flipped(ValidIO(UInt(XLEN.W)))
}

class VCsrIO(implicit p: Parameters) extends VectorBaseBundle {
  val vtype = new VCSRWithVtypeRenameIO
  val robWb = new VCSRWithRobIO
  val vstart = Output(UInt(7.W))
  val vcsr    = Output(UInt(3.W))
}

class VtypeStruct(implicit p: Parameters) extends XSBundle {
  val vill      = UInt(1.W)
  val reserved  = UInt((XLEN-9).W)
  val vma       = UInt(1.W)
  val vta       = UInt(1.W)
  val vsew      = UInt(3.W)
  val vlmul     = UInt(3.W)
  assert(this.getWidth == XLEN)

  def vset_parse(vtype: UInt): Unit = {
    this.vlmul    := vtype(2, 0)
    this.vsew     := vtype(5, 3)
    this.vta      := vtype(6)
    this.vma      := vtype(7)
    this.reserved := 0.U
    this.vill := 0.U
  }

  def check_illegal: Bool = {
    val vsew_illegal = vsew(2) === 1.U
    val vlmul_illegal = vlmul === 4.U
    vsew_illegal || vlmul_illegal
  }
}

class VSetFu(implicit p: Parameters) extends XSModule with HasXSParameter {
  val io = IO(new Bundle {
    val src         = Vec(2, Input(UInt(XLEN.W)))
    val rs1IsX0     = Input(Bool())
    val rdIsX0      = Input(Bool())
    val vsetType    = Input(UInt(3.W)) //one-hot, 001,010,100->vsetivli, vsetvli, vsetvl
    val vlOld       = Input(UInt(XLEN.W))
    val vtypeNew    = Output(UInt(XLEN.W))
    val vlNew       = Output(UInt(XLEN.W))
  })

  val (src1, src2) = (io.src(0), io.src(1))
  val uimm    = src2(4, 0)
  val zimm_ii = src2(14, 5)
  val zimm_i  = src2(10, 0)

  val vtypeSetivli  = Wire(new VtypeStruct).vset_parse(zimm_ii)
  val vtypeSetvli   = Wire(new VtypeStruct).vset_parse(zimm_i)
  val vtypeSetvl    = Wire(new VtypeStruct).vset_parse(src2)

  val avl   = Wire(UInt((log2Up(VLEN) + 1).W))
  val vtype = Wire(new VtypeStruct)

  vtype := Mux1H(io.vsetType, Seq(
    zimm_ii, zimm_i, src2
  )).asTypeOf(new VtypeStruct)

  val vlmul = vtype.vlmul
  val vsew  = vtype.vsew

  val VLMAX = Wire(UInt(7.W))
  VLMAX := MuxCase(0.U, Seq(
      (vlmul === 0.U) -> ((VLEN >> 3).U >> vsew),
      (vlmul === 1.U) -> ((VLEN >> 2).U >> vsew),
      (vlmul === 2.U) -> ((VLEN >> 1).U >> vsew),
      (vlmul === 3.U) -> ((VLEN).U      >> vsew),
      (vlmul === 5.U) -> ((VLEN >> 6).U >> vsew),
      (vlmul === 6.U) -> ((VLEN >> 5).U >> vsew),
      (vlmul === 7.U) -> ((VLEN >> 4).U >> vsew)
    )
  )

  val avl_hasRs1 = Wire(UInt((log2Up(VLEN) + 1).W))
  avl_hasRs1 := Mux(!io.rs1IsX0, src1, Mux(!io.rdIsX0, VLMAX, io.vlOld))
  avl := Mux((io.vsetType === "b001".asUInt), uimm, avl_hasRs1)
  io.vtypeNew := Cat(vtype.vill, vtype.vma, vtype.vta, vtype.vsew, vtype.vlmul)
  val vlValue = Mux(!io.rs1IsX0, avl, Mux(io.vlOld > VLMAX, VLMAX, io.vlOld))
  io.vlNew := vlValue(7, 0)
}
