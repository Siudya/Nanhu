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

package xiangshan.backend.execute.exucx

import org.chipsalliance.cde.config.Parameters
import chisel3._
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.FuType
import xiangshan.backend.execute.exu.{ExuType, AluExu, MulExu, DivExu, NewMiscExu}
import xiangshan._
import chisel3.util._
import xiangshan.backend.execute.fu._

class DivMiscComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex {
  val misc = LazyModule(new NewMiscExu(id, "DivMiscComplex", bypassNum))
  val div = LazyModule(new DivExu(id, "DivMiscComplex", bypassNum))

  misc.issueNode :*= issueNode
  div.issueNode :*= issueNode

  writebackNode :=* misc.writebackNode
  writebackNode :=* div.writebackNode

  lazy val module = new DivMiscComplexImp(this, id, bypassNum)
}

class DivMiscComplexImp(outer: DivMiscComplex, id: Int, bypassNum: Int) extends BasicExuComplexImp(outer, bypassNum){
    require(outer.issueNode.in.length == 1)
    require(outer.issueNode.out.length == 2)
    private val issueIn = outer.issueNode.in.head._1
    private val issueMisc = outer.issueNode.out.filter(_._2._2.exuType == ExuType.misc).head._1
    private val issueDiv = outer.issueNode.out.filter(_._2._2.exuType == ExuType.div).head._1

    val io = IO(new Bundle {
      val issueToMou = Decoupled(new ExuInput)
      val writebackFromMou = Flipped(Decoupled(new ExuOutput))

      val issueToFence = DecoupledIO(new FuInput(p(XSCoreParamsKey).XLEN))
      val writebackFromFence = Flipped(DecoupledIO(new FuOutput(p(XSCoreParamsKey).XLEN)))
      val redirectFromFence = Flipped(ValidIO(new Redirect))

      val issueToCSR = DecoupledIO(new FuInput(p(XSCoreParamsKey).XLEN))
      val writebackFromCSR = Flipped(DecoupledIO(new FuOutput(p(XSCoreParamsKey).XLEN)))
      val redirectFromCSR = Flipped(ValidIO(new Redirect))
      val csrIsPerfCnt = Input(Bool())
    })

    issueMisc <> issueIn
    outer.misc.module.io.bypassIn     := bypassIn
    outer.misc.module.redirectIn      := DontCare
    outer.misc.module.io.csrIsPerfCnt := io.csrIsPerfCnt

    io.issueToMou <> outer.misc.module.io.issueToMou
    outer.misc.module.io.writebackFromMou <> io.writebackFromMou

    io.issueToFence <> outer.misc.module.io.issueToFence
    io.writebackFromFence <> outer.misc.module.io.writebackFromFence
    outer.misc.module.io.redirectFromFence := io.redirectFromFence

    io.issueToCSR <> outer.misc.module.io.issueToCSR
    io.writebackFromCSR <> outer.misc.module.io.writebackFromCSR
    outer.misc.module.io.redirectFromCSR := io.redirectFromCSR

    issueDiv <> issueIn
    outer.div.module.io.bypassIn := bypassIn
    outer.div.module.redirectIn := redirectIn

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.div, issueDiv.issue.ready, issueMisc.issue.ready)

    private val issueFuHit = outer.issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_|_)
    when(issueIn.issue.valid){assert(issueFuHit)}
  }