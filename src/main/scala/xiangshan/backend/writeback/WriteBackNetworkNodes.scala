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
package xiangshan.backend.writeback
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util._
import xiangshan.ExuOutput
import xiangshan.backend.execute.exu.ExuConfig

object WriteBackSinkType{
  def regFile = 0
  def rob = 3
  def intRs = 4
  def memRs = 5
  def fpRs = 6
  private def rfList = Seq(regFile)
  private def rsList = Seq(intRs, memRs, fpRs)
  def isRf(in:Int) = rfList.contains(in)
  def isRs(in:Int) = rsList.contains(in)
}

case class WriteBackSinkParam
(
  val name: String,
  val sinkType: Int,
){
  def isRegFile: Boolean = sinkType == WriteBackSinkType.regFile
  def isRob: Boolean = sinkType == WriteBackSinkType.rob
  def isIntRs: Boolean = sinkType == WriteBackSinkType.intRs
  def isMemRs: Boolean = sinkType == WriteBackSinkType.memRs
  def isFpRs: Boolean = sinkType == WriteBackSinkType.fpRs
  def isLegal: Boolean = isRegFile ||isRob ||isIntRs ||isMemRs ||isFpRs
  def needWriteback: Boolean = isRegFile || isRob
}

object WriteBackNetworkNodeInwardImpl extends InwardNodeImp[ExuConfig, Option[ExuConfig], (ExuConfig, Parameters), Valid[ExuOutput]]{
  override def edgeI(pd: ExuConfig, pu: Option[ExuConfig], p: Parameters, sourceInfo: SourceInfo): (ExuConfig, Parameters) = (pd, p)
  override def bundleI(ei: (ExuConfig, Parameters)): ValidIO[ExuOutput] = Valid(new ExuOutput()(ei._2))
  override def render(e: (ExuConfig, Parameters)): RenderedEdge = RenderedEdge("#0000ff",e._1.name + " writeback")
}
object WriteBackNetworkNodeOutwardImpl extends OutwardNodeImp[Seq[ExuConfig], WriteBackSinkParam, (WriteBackSinkParam, Seq[ExuConfig], Parameters), Vec[Valid[ExuOutput]]]{
  override def edgeO(pd: Seq[ExuConfig], pu: WriteBackSinkParam, p: Parameters, sourceInfo: SourceInfo): (WriteBackSinkParam, Seq[ExuConfig], Parameters) = {
    require(pu.isLegal)
    val resPd = if (pu.isRegFile) {
      pd.filter(cfg => cfg.writeIntRf || cfg.writeFpRf || cfg.writeVecRf )
    } else if (pu.isIntRs) {
      pd.filter(_.wakeUpIntRs)
    } else if (pu.isMemRs) {
      pd.filter(_.wakeUpMemRs)
    } else if (pu.isFpRs) {
      pd.filter(_.wakeUpFpRs)
    } else {
      pd
    }
    (pu,resPd,p)
  }
  override def bundleO(eo: (WriteBackSinkParam, Seq[ExuConfig], Parameters)): Vec[ValidIO[ExuOutput]] = Vec(eo._2.length, Valid(new ExuOutput()(eo._3)))
}
object WriteBackSinkNodeImpl extends SimpleNodeImp[Seq[ExuConfig], WriteBackSinkParam, (Seq[ExuConfig], Parameters), Vec[Valid[ExuOutput]]]{
  override def edge(pd: Seq[ExuConfig], pu: WriteBackSinkParam, p: Parameters, sourceInfo: SourceInfo): (Seq[ExuConfig], Parameters) = {
    require(pu.isLegal)
    val resPd = if (pu.isRegFile) {
      pd.filter(cfg => cfg.writeIntRf || cfg.writeFpRf || cfg.writeVecRf)
    } else if (pu.isIntRs) {
      pd.filter(_.wakeUpIntRs)
    } else if (pu.isMemRs) {
      pd.filter(_.wakeUpMemRs)
    } else if (pu.isFpRs) {
      pd.filter(_.wakeUpFpRs)
    } else {
      pd
    }
    (resPd,p)
  }
  override def bundle(e: (Seq[ExuConfig], Parameters)): Vec[ValidIO[ExuOutput]] = Vec(e._1.length, Valid(new ExuOutput()(e._2)))
  override def render(e: (Seq[ExuConfig], Parameters)): RenderedEdge = RenderedEdge(colour = "#0000ff",label = "writeback")
}

class WriteBackNetworkNode(implicit valName: ValName) extends MixedNexusNode(WriteBackNetworkNodeInwardImpl, WriteBackNetworkNodeOutwardImpl)(
  dFn = {p:Seq[ExuConfig] => p},
  uFn = {p:Seq[WriteBackSinkParam] => None},
  inputRequiresOutput = false,
  outputRequiresInput = false
)
class WriteBackSinkNode(param:WriteBackSinkParam)(implicit valName: ValName) extends SinkNode(WriteBackSinkNodeImpl)(Seq(param))