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
import xiangshan.backend.execute.exu.{ExuType, AluExu, MulExu, DivExu}
import xiangshan._
import chisel3.util._

class AluMulDivStdComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluMulDivStdComplex", bypassNum))
  val mul = LazyModule(new MulExu(id, "AluMulDivStdComplex", bypassNum))
  val div = LazyModule(new DivExu(id, "AluMulDivStdComplex", bypassNum))
  //TODO: now has not the FakeStd

  alu.issueNode :*= issueNode
  mul.issueNode :*= issueNode
  div.issueNode :*= issueNode

  writebackNode :=* alu.writebackNode
  writebackNode :=* mul.writebackNode
  writebackNode :=* div.writebackNode

  lazy val module = new AluMulDivStdComplexImp(this, id, bypassNum)
}

class AluMulDivStdComplexImp(outer:AluMulDivStdComplex, id:Int, bypassNum:Int) extends BasicExuComplexImp(outer, bypassNum){
    require(outer.issueNode.in.length == 1)
    require(outer.issueNode.out.length == 3)
    private val issueIn = outer.issueNode.in.head._1
    private val issueAlu = outer.issueNode.out.filter(_._2._2.exuType == ExuType.alu).head._1
    private val issueMul = outer.issueNode.out.filter(_._2._2.exuType == ExuType.mul).head._1
    private val issueDiv = outer.issueNode.out.filter(_._2._2.exuType == ExuType.div).head._1

    val io = IO(new Bundle {
      val bypassOut = Output(Valid(new ExuOutput))
      val csr_frm: UInt = Input(UInt(3.W))
    })

    issueAlu <> issueIn
    outer.alu.module.io.bypassIn := bypassIn
    outer.alu.module.redirectIn := redirectIn

    issueMul <> issueIn
    outer.mul.module.io.bypassIn := bypassIn
    outer.mul.module.io.csr_frm := RegNext(io.csr_frm)
    outer.mul.module.redirectIn := redirectIn
    io.bypassOut := outer.mul.module.io.bypassOut

    issueDiv <> issueIn
    outer.div.module.io.bypassIn := bypassIn
    outer.div.module.redirectIn := redirectIn

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, 
                                Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.div, issueDiv.issue.ready, issueMul.issue.ready))

    private val issueFuHit = outer.issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_|_)
    when(issueIn.issue.valid){assert(issueFuHit)}
  }