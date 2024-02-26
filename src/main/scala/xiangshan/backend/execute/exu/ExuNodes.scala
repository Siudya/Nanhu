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
package xiangshan.backend.execute.exu
import org.chipsalliance.cde.config.Parameters
import chisel3.experimental.SourceInfo
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
  override def bundle(e: (RsParam, ExuConfig, Parameters)): IssueBundle = new IssueBundle()(e._3)
  override def render(e: (RsParam, ExuConfig, Parameters)) = RenderedEdge("#00ff00", e._2.name)
}
object ExuOutwardImpl extends SimpleNodeImp[ExuConfig, Option[ExuConfig],(ExuConfig, Parameters),Valid[ExuOutput]]{
  override def edge(pd: ExuConfig, pu: Option[ExuConfig], p: Parameters, sourceInfo: SourceInfo):(ExuConfig,Parameters) = (pd,p)
  override def bundle(eo: (ExuConfig,Parameters)): Valid[ExuOutput] = Valid(new ExuOutput()(eo._2))
  override def render(e: (ExuConfig,Parameters)) = RenderedEdge("#0000ff", e._1.name)
}

class ExuInputNode(exuConfig: ExuConfig)(implicit valName: ValName) extends SinkNode(ExuInwardImpl)(Seq(exuConfig))
class ExuOutputNode(exuConfig: ExuConfig)(implicit valName: ValName) extends SourceNode(ExuOutwardImpl)(Seq(exuConfig))
class ExuOutputMultiSinkNode(exuConfigs: Seq[ExuConfig])(implicit valName: ValName) extends SourceNode(ExuOutwardImpl)(exuConfigs)