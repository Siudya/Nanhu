package xiangshan.backend.execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.FuType
import xiangshan.backend.execute.exu.{ExuType, FdivExu, FmacExu}
import xs.utils.Assertion.xs_assert

class FmaDivComplex (id: Int)(implicit p:Parameters) extends BasicExuComplex{
  val fmac = LazyModule(new FmacExu(id,"FmaDivComplex"))
  val fdiv = LazyModule(new FdivExu(id, "FmaDivComplex"))
  fmac.issueNode :*= issueNode
  fdiv.issueNode :*= issueNode
  writebackNode :=* fmac.writebackNode
  writebackNode :=* fdiv.writebackNode
  lazy val module = new BasicExuComplexImp(this, 0) {
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    private val issueIn = issueNode.in.head._1
    private val issueFmac = issueNode.out.filter(_._2._2.exuType == ExuType.fmac).head._1
    private val issueFdiv = issueNode.out.filter(_._2._2.exuType == ExuType.fdiv).head._1

    issueFmac <> issueIn
    fmac.module.redirectIn := redirectIn

    issueFdiv <> issueIn
    fdiv.module.redirectIn := redirectIn

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.fmac, issueFmac.issue.ready, issueFmac.issue.ready)
    private val issueFuHit = issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_ | _)
    xs_assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
