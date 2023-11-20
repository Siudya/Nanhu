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

import org.chipsalliance.cde.config.Parameters
import chisel3.util.Valid
import chisel3.{Input, Vec}
import xiangshan.{ExuOutput, Redirect, XSCoreParamsKey}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

abstract class BasicExuBlock(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuBlockIssueNode
  val writebackNode = new ExuBlockWritebackNode
  protected val mulNum:Int = p(XSCoreParamsKey).exuParameters.AluMulCnt
  protected val divNum:Int = p(XSCoreParamsKey).exuParameters.AluDivCnt
  protected val miscNum:Int = p(XSCoreParamsKey).exuParameters.AluMiscCnt
  protected val jmpNum:Int = p(XSCoreParamsKey).exuParameters.JmpCnt
  protected val fmaNum:Int = p(XSCoreParamsKey).exuParameters.FmaCnt
  protected val fmaMiscNum:Int = p(XSCoreParamsKey).exuParameters.FmaMiscCnt
  protected val fmaDivNum:Int = p(XSCoreParamsKey).exuParameters.FmaDivCnt
  protected val loadNum:Int = p(XSCoreParamsKey).exuParameters.LduCnt
  protected val storeNum:Int = p(XSCoreParamsKey).exuParameters.StuCnt
  override def module:BasicExuBlockImp
}

class BasicExuBlockImp(outer:BasicExuBlock) extends LazyModuleImp(outer){
  val redirectIn = IO(Input(Valid(new Redirect)))
  outer.writebackNode.in.zip(outer.writebackNode.out).foreach({
    case (source, sink) =>
      sink._1 := source._1
  })
  outer.issueNode.in.zip(outer.issueNode.out).foreach({
    case (source, sink) =>
      sink._1 <> source._1
  })
}