package xiangshan.backend.execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.backend.execute.exu.FmacExu

class FmacComplex(id: Int)(implicit p:Parameters) extends BasicExuComplex{
  val fmac = LazyModule(new FmacExu(id,"FmacComplex"))
  fmac.issueNode :*= issueNode
  writebackNode :=* fmac.writebackNode
  lazy val module = new BasicExuComplexImp(this, 0){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 1)
    val csr_frm: UInt = IO(Input(UInt(3.W)))
    private val issueIn = issueNode.in.head._1
    private val issueRouted = issueNode.out.map(_._1)
    issueRouted.foreach(_ <> issueIn)
    fmac.module.redirectIn := redirectIn
    fmac.module.csr_frm := csr_frm
  }
}
