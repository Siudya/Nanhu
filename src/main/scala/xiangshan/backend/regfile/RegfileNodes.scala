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
package xiangshan.backend.regfile

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{MixedNexusNode, _}
import chisel3._
import chisel3.experimental.SourceInfo
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
  override def bundleI(ei: (RsParam, Seq[ExuComplexParam], Parameters)): Vec[IssueBundle] = Vec(ei._2.length, new IssueBundle()(ei._3))
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
  override def bundleO(eo: (RsParam, ExuComplexParam, Parameters)): IssueBundle = new IssueBundle()(eo._3)
}

class RegFileNode(implicit valName: ValName) extends MixedNexusNode(
  inner = RegFileNodeInwardImpl, outer = RegFileNodeOutwardImpl
)(
  { pd => pd }, { pu => pu }
)