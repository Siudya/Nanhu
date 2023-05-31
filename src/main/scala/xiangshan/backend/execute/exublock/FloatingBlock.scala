package xiangshan.backend.execute.exublock

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import xiangshan.backend.execute.exucx.{FmaDivComplex, FmaMiscComplex, FmacComplex}
import freechips.rocketchip.diplomacy.LazyModule

class FloatingBlock(implicit p:Parameters) extends BasicExuBlock{
  private val fmacs = Seq.tabulate(fmaNum)(idx => LazyModule(new FmacComplex(idx)))
  private val fmacDivs = Seq.tabulate(fmaDivNum)(idx => LazyModule(new FmaDivComplex(idx)))
  private val fmaMiscs = Seq.tabulate(fmaMiscNum)(idx => LazyModule(new FmaMiscComplex(idx)))
  private val fpComplexes = fmacs ++ fmacDivs ++ fmaMiscs
  fpComplexes.foreach(exucx => {
    exucx.issueNode :*= issueNode
    writebackNode :=* exucx.writebackNode
  })
  lazy val module = new BasicExuBlockImp(this){
    val io = IO(new Bundle{
      val csr_frm: UInt = Input(UInt(3.W))
    })
    fpComplexes.foreach(_.module.redirectIn := redirectIn)
    fmacs.foreach(_.module.csr_frm := RegNext(io.csr_frm))
    fmacDivs.foreach(_.module.csr_frm := RegNext(io.csr_frm))
    fmaMiscs.foreach(_.module.csr_frm := RegNext(io.csr_frm))
  }
}
