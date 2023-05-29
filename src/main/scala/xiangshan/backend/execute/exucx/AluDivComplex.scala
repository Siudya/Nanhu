package xiangshan.backend.execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.FuType
import xiangshan.backend.execute.exu.{AluExu, DivExu, ExuType}
import xs.utils.Assertion.xs_assert

class AluDivComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluDivComplex", bypassNum))
  val div = LazyModule(new DivExu(id, "AluDivComplex", bypassNum))
  alu.issueNode :*= issueNode
  div.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* div.writebackNode
  lazy val module = new BasicExuComplexImp(this, bypassNum){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    private val issueIn = issueNode.in.head._1
    private val issueAlu = issueNode.out.filter(_._2._2.exuType == ExuType.alu).head._1
    private val issueDiv = issueNode.out.filter(_._2._2.exuType == ExuType.div).head._1

    issueAlu <> issueIn
    alu.module.io.bypassIn := bypassIn
    alu.module.redirectIn := redirectIn

    issueDiv <> issueIn
    div.module.io.bypassIn := bypassIn
    div.module.redirectIn := redirectIn

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, issueDiv.issue.ready)

    private val issueFuHit = issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_|_)
    xs_assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
