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
package xiangshan.backend.issue
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import chisel3.internal.sourceinfo.SourceInfo
import xiangshan.MicroOp
import xiangshan.backend.execute.exucx.ExuComplexParam


object RsIssueNodeImpl extends SimpleNodeImp[RsParam, Seq[ExuComplexParam], (RsParam, Seq[ExuComplexParam], Parameters), Vec[IssueBundle]]{
  override def edge(pd: RsParam, pu: Seq[ExuComplexParam], p: Parameters, sourceInfo: SourceInfo): (RsParam, Seq[ExuComplexParam], Parameters) = {
    require(pd.isLegal)
    if (pd.isIntRs) {
      (pd, pu.filter(_.isIntType), p)
    } else if (pd.isMemRs) {
      (pd, pu.filter(_.isMemType), p)
    } else if (pd.isVecRs) {
      (pd, pu.filter(_.isVecType), p)
    } else {
      (pd, pu.filter(_.isFpType), p)
    }
  }
  override def bundle(e: (RsParam, Seq[ExuComplexParam], Parameters)): Vec[IssueBundle] = Vec(e._2.length, new IssueBundle()(e._3))
  override def render(e: (RsParam, Seq[ExuComplexParam], Parameters)): RenderedEdge = {
    RenderedEdge("#00ff00", e._1.TypeName + "Issue")
  }
}
object RsDispatchNodeImpl extends SimpleNodeImp[Option[RsParam], RsParam, (RsParam, Parameters), Vec[DecoupledIO[MicroOp]]] {
  override def edge(pd: Option[RsParam], pu: RsParam, p: Parameters, sourceInfo: SourceInfo): (RsParam, Parameters) = (pu, p)
  override def bundle(e: (RsParam, Parameters)): Vec[DecoupledIO[MicroOp]] = Vec(e._1.bankNum, DecoupledIO(new MicroOp()(e._2)))
  override def render(e: (RsParam, Parameters)): RenderedEdge = RenderedEdge("#ff0000", e._1.name)
}

class RsIssueNode(param:RsParam)(implicit valName: ValName) extends SourceNode(RsIssueNodeImpl)(Seq(param))

class RsDispatchNode(paramSeq:RsParam)(implicit valName: ValName) extends SinkNode(RsDispatchNodeImpl)(Seq(paramSeq))

class DqDispatchNode(implicit valName: ValName) extends SourceNode(RsDispatchNodeImpl)(Seq.fill(3)(None))