/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/
/***************************************************************************************
 * Author: Liang Sen
 * E-mail: liangsen20z@ict.ac.cn
 * Date: 2023-06-19
 ****************************************************************************************/
package xiangshan.backend.execute.exucx
import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.diplomacy._
import xiangshan.backend.execute.exu.{ExuConfig, ExuOutwardImpl, ExuType}
import xiangshan.backend.issue.{IssueBundle, RsParam}

case class ExuComplexParam
(
  val id:Int,
  val exuConfigs: Seq[ExuConfig]
){
  val name: String = exuConfigs.head.complexName
  val hasJmp: Boolean = exuConfigs.map(_.exuType == ExuType.jmp).reduce(_ || _)
  val hasAlu: Boolean = exuConfigs.map(_.exuType == ExuType.alu).reduce(_ || _)
  val hasMul: Boolean = exuConfigs.map(_.exuType == ExuType.mul).reduce(_ || _)
  val hasDiv: Boolean = exuConfigs.map(_.exuType == ExuType.div).reduce(_ || _)
  val hasFmac: Boolean = exuConfigs.map(_.exuType == ExuType.fmac).reduce(_ || _)
  val hasFmisc: Boolean = exuConfigs.map(_.exuType == ExuType.fmisc).reduce(_ || _)
  val hasFdiv: Boolean = exuConfigs.map(_.exuType == ExuType.fdiv).reduce(_ || _)
  val hasLoad: Boolean = exuConfigs.map(_.exuType == ExuType.ldu).reduce(_ || _)
  val hasSta: Boolean = exuConfigs.map(_.exuType == ExuType.sta).reduce(_ || _)
  val hasStd: Boolean = exuConfigs.map(_.exuType == ExuType.std).reduce(_ || _)
  val hasVred:Boolean = exuConfigs.map(_.exuType == ExuType.vred).reduce(_ || _)
  val hasVmisc:Boolean = exuConfigs.map(_.exuType == ExuType.vmisc).reduce(_ || _)
  val hasVfp:Boolean = exuConfigs.map(_.exuType == ExuType.vfp).reduce(_ || _)
  val hasVint:Boolean = exuConfigs.map(_.exuType == ExuType.vint).reduce(_ || _)
  val isIntType:Boolean = exuConfigs.head.isIntType
  val isFpType:Boolean = exuConfigs.head.isFpType
  val isMemType:Boolean = exuConfigs.head.isMemType
  val isVecType:Boolean = exuConfigs.head.isVecType
  val intSrcNum:Int = exuConfigs.map(_.intSrcNum).max
  val fpSrcNum:Int = exuConfigs.map(_.fpSrcNum).max

  val isAluDiv:Boolean = hasAlu && hasDiv
  val isAluJmp:Boolean = hasAlu && hasJmp
  val isAluMul:Boolean = hasAlu && hasMul
  val isFmac:Boolean = hasFmac && !hasFdiv && !hasFmisc
  val isFmaDiv:Boolean = hasFmac && hasFdiv
  val isFmaMisc:Boolean = hasFmac && hasFmisc
  val isSta:Boolean = hasSta
  val isStd:Boolean = hasStd
  val isLdu:Boolean = hasLoad

  val needToken:Boolean = exuConfigs.map(_.needToken).reduce(_||_)

  val readIntegerRegfile:Boolean = isAluDiv || isAluJmp || isAluMul || hasSta || hasStd || hasLoad || hasVmisc || hasVint
  val readFloatingRegfile:Boolean = isFmac || isFmaDiv || isFmaMisc || hasStd || hasVfp
  val readVectorRegfile:Boolean = isVecType || hasLoad || hasStd || hasSta

  override def toString:String = s"${name} #${id} intSrcNum:${intSrcNum} fpSrcNum:${fpSrcNum} " + exuConfigs.map(_.toString).reduce(_++_)
}
object ExuComplexIssueInwardNodeImpl extends InwardNodeImp[Seq[RsParam], ExuComplexParam, (RsParam, ExuComplexParam, Parameters), IssueBundle]{
  override def edgeI(pd: Seq[RsParam], pu: ExuComplexParam, p: Parameters, sourceInfo: SourceInfo): (RsParam, ExuComplexParam, Parameters) = {
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
  override def bundleI(ei: (RsParam, ExuComplexParam, Parameters)): IssueBundle = new IssueBundle()(ei._3)
  override def render(ei: (RsParam, ExuComplexParam, Parameters)): RenderedEdge = RenderedEdge("#0000ff", ei._2.exuConfigs.map(_.name).reduce(_++_))
}
object ExuComplexIssueOutwardNodeImpl extends OutwardNodeImp[Seq[RsParam], ExuConfig, (RsParam, ExuConfig, Parameters), IssueBundle]{
  override def edgeO(pd: Seq[RsParam], pu: ExuConfig, p: Parameters, sourceInfo: SourceInfo): (RsParam, ExuConfig, Parameters) = {
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

  override def bundleO(eo: (RsParam, ExuConfig, Parameters)): IssueBundle = new IssueBundle()(eo._3)
}
class ExuComplexIssueNode(implicit valName: ValName) extends
  MixedNexusNode(inner = ExuComplexIssueInwardNodeImpl, outer = ExuComplexIssueOutwardNodeImpl)(
    dFn = {p:Seq[Seq[RsParam]] => {
      require(p.length == 1)
      p.head
    }},
    uFn = {p:Seq[ExuConfig] => ExuComplexParam(p.head.id, p)}
  )

class ExuComplexWritebackNode(implicit valName: ValName) extends
  AdapterNode(ExuOutwardImpl)({p => p}, {p => p})