package xiangshan.backend.regfile

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{MixedNexusNode, _}
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import xiangshan.backend.execute.exucx.ExuComplexParam
import xiangshan.backend.issue.{IssueBundle, RsParam}

object RegFileNodeInwardImpl extends InwardNodeImp[RsParam,Seq[ExuComplexParam],(RsParam, Seq[ExuComplexParam], Parameters),Vec[IssueBundle]]{

  override def edgeI(pd: RsParam, pu: Seq[ExuComplexParam], p: Parameters, sourceInfo: SourceInfo): (RsParam, Seq[ExuComplexParam], Parameters) = {
    require(pd.isLegal)
    if(pd.isIntRs){
      (pd, pu.filter(_.isIntType), p)
    } else if(pd.isMemRs) {
      (pd, pu.filter(_.isMemType), p)
    } else if(pd.isVecRs) {
      (pd, pu.filter(_.isVecType), p)
    } else {
      (pd, pu.filter(_.isFpType), p)
    }
  }
  override def bundleI(ei: (RsParam, Seq[ExuComplexParam], Parameters)): Vec[IssueBundle] = Vec(ei._2.length, new IssueBundle(ei._1.bankNum, ei._1.entriesNum)(ei._3))
  override def render(e: (RsParam, Seq[ExuComplexParam], Parameters)): RenderedEdge = RenderedEdge("#0000ff", e._1.TypeName + "Issue")
}
object RegFileNodeOutwardImpl extends OutwardNodeImp[Seq[RsParam], ExuComplexParam, (RsParam, ExuComplexParam, Parameters), IssueBundle]{
  override def edgeO(pd: Seq[RsParam], pu: ExuComplexParam, p: Parameters, sourceInfo: SourceInfo): (RsParam, ExuComplexParam, Parameters) = {
    require(pu.isFpType || pu.isVecType || pu.isIntType || pu.isMemType)
    if(pu.isFpType){
      (pd.filter(_.isFpRs).head, pu, p)
    } else if(pu.isVecType) {
      (pd.filter(_.isVecRs).head, pu, p)
    } else if (pu.isIntType) {
      (pd.filter(_.isIntRs).head, pu, p)
    } else {
      (pd.filter(_.isMemRs).head, pu, p)
    }
  }
  override def bundleO(eo: (RsParam, ExuComplexParam, Parameters)): IssueBundle = new IssueBundle(eo._1.bankNum, eo._1.entriesNum)(eo._3)
}

class RegFileNode(implicit valName: ValName) extends MixedNexusNode(
  inner = RegFileNodeInwardImpl, outer = RegFileNodeOutwardImpl
)(
  { pd => pd }, { pu => pu }
)