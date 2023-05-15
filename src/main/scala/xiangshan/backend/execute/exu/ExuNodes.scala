package xiangshan.backend.execute.exu
import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import freechips.rocketchip.diplomacy._
import xiangshan.ExuOutput
import xiangshan.backend.issue.{IssueBundle, RsParam}
object ExuInwardImpl extends SimpleNodeImp[Seq[RsParam],ExuConfig,(RsParam, ExuConfig, Parameters),IssueBundle]{
  override def edge(pd: Seq[RsParam], pu: ExuConfig, p: Parameters, sourceInfo: SourceInfo):(RsParam, ExuConfig, Parameters) = {
    require(pu.isFpType || pu.isVecType || pu.isIntType || pu.isMemType)
    if (pu.isFpType) {
      (pd.filter(_.isFpRs).head, pu, p)
    } else if (pu.isVecType) {
      (pd.filter(_.isVecRs).head, pu, p)
    } else if (pu.isIntType) {
      (pd.filter(_.isIntRs).head, pu, p)
    } else {
      (pd.filter(_.isMemRs).head, pu, p)
    }
  }
  override def bundle(e: (RsParam, ExuConfig, Parameters)): IssueBundle = new IssueBundle(e._1.bankNum, e._1.entriesNum)(e._3)
  override def render(e: (RsParam, ExuConfig, Parameters)) = RenderedEdge("#00ff00", e._2.name)
}
object ExuOutwardImpl extends SimpleNodeImp[ExuConfig, Option[ExuConfig],(ExuConfig, Parameters),Valid[ExuOutput]]{
  override def edge(pd: ExuConfig, pu: Option[ExuConfig], p: Parameters, sourceInfo: SourceInfo):(ExuConfig,Parameters) = (pd,p)
  override def bundle(eo: (ExuConfig,Parameters)): Valid[ExuOutput] = Valid(new ExuOutput()(eo._2))
  override def render(e: (ExuConfig,Parameters)) = RenderedEdge("#0000ff", e._1.name)
}

class ExuInputNode(exuConfig: ExuConfig)(implicit valName: ValName) extends SinkNode(ExuInwardImpl)(Seq(exuConfig))
class ExuOutputNode(exuConfig: ExuConfig)(implicit valName: ValName) extends SourceNode(ExuOutwardImpl)(Seq(exuConfig))