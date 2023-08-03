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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.issue.IssueBundle
import xiangshan.{ExuInput, ExuOutput, Redirect, SrcType}

abstract class BasicExu(implicit p:Parameters) extends LazyModule{
  def issueNode: ExuInputNode
  def writebackNode: ExuOutputNode
  def module:BasicExuImpl
}

abstract class BasicExuImpl(outer:BasicExu) extends LazyModuleImp(outer){
  val redirectIn = IO(Input(Valid(new Redirect)))

  def bypassSigGen(bypassIn:Seq[Valid[ExuOutput]], issuePort:IssueBundle, hasBypass:Boolean):Valid[ExuInput] = {
    val finalIssueSignals = Wire(Valid(new ExuInput))
    finalIssueSignals.valid := issuePort.issue.valid && !issuePort.issue.bits.uop.robIdx.needFlush(redirectIn)
    finalIssueSignals.bits.uop := issuePort.issue.bits.uop
    finalIssueSignals.bits.src := issuePort.issue.bits.src
    if(hasBypass) {
      val bypass = bypassIn
      val bypassData = bypass.map(_.bits.data)
      val bypassSrc0Hits = bypass.map(elm => elm.valid && elm.bits.uop.pdest === issuePort.issue.bits.uop.psrc(0))
      val bypassSrc1Hits = bypass.map(elm => elm.valid && elm.bits.uop.pdest === issuePort.issue.bits.uop.psrc(1))
      val bypassSrc0Valid = Cat(bypassSrc0Hits).orR && issuePort.issue.bits.uop.ctrl.srcType(0) === SrcType.reg
      val bypassSrc0Data = Mux1H(bypassSrc0Hits, bypassData)
      val bypassSrc1Valid = Cat(bypassSrc1Hits).orR && issuePort.issue.bits.uop.ctrl.srcType(1) === SrcType.reg
      val bypassSrc1Data = Mux1H(bypassSrc1Hits, bypassData)
      assert(PopCount(Cat(bypassSrc0Hits)) === 1.U || Cat(bypassSrc0Hits) === 0.U)
      assert(PopCount(Cat(bypassSrc1Hits)) === 1.U || Cat(bypassSrc1Hits) === 0.U)
      finalIssueSignals.bits.src(0) := Mux(bypassSrc0Valid, bypassSrc0Data, issuePort.issue.bits.src(0))
      finalIssueSignals.bits.src(1) := Mux(bypassSrc1Valid, bypassSrc1Data, issuePort.issue.bits.src(1))
    }
    finalIssueSignals
  }
}
