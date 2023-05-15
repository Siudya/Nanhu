package xiangshan.backend.execute.exublock

import chipsalliance.rocketchip.config.Parameters
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
    fpComplexes.foreach(_.module.redirectIn := redirectIn)
  }
}
