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
package xiangshan.backend.execute.exublock

import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.SourceInfo
import xiangshan.backend.execute.exu.ExuConfig
import xiangshan.backend.execute.exucx.{ExuComplexParam, ExuComplexWritebackNode}
import freechips.rocketchip.diplomacy.{AdapterNode, RenderedEdge, SimpleNodeImp, SinkNode, ValName}
import xiangshan.backend.issue.{IssueBundle, RsParam}

object ExuBlockIssueNodeImpl extends SimpleNodeImp[Seq[RsParam], ExuComplexParam, (RsParam, ExuComplexParam, Parameters), IssueBundle]{
  override def edge(pd: Seq[RsParam], pu: ExuComplexParam, p: Parameters, sourceInfo: SourceInfo): (RsParam, ExuComplexParam, Parameters) = {
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
  override def bundle(e: (RsParam, ExuComplexParam, Parameters)): IssueBundle = new IssueBundle()(e._3)
  override def render(e: (RsParam, ExuComplexParam, Parameters)): RenderedEdge = RenderedEdge("#0000ff", e._1.TypeName + "Issue")
}

class ExuBlockWritebackNode(implicit valName: ValName) extends ExuComplexWritebackNode

class ExuBlockIssueNode(implicit valName: ValName) extends
  AdapterNode(ExuBlockIssueNodeImpl)({p => p}, {p => p})

class MemoryBlockIssueNode(cfg:(ExuConfig, Int))(implicit valName: ValName) extends SinkNode(ExuBlockIssueNodeImpl)(Seq(ExuComplexParam(cfg._2, Seq(cfg._1))))