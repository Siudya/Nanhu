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
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.backend.execute.exu.{AluExu, ExuType, FenceIO, JmpCsrExu}
import xiangshan.backend.execute.fu.csr.CSRFileIO
import xiangshan.{ExuInput, ExuOutput, FuType}

class AluJmpComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluJmpComplex", bypassNum))
  val jmp = LazyModule(new JmpCsrExu(id, "AluJmpComplex", bypassNum))
  alu.issueNode :*= issueNode
  jmp.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* jmp.writebackNode
  lazy val module = new BasicExuComplexImp(this, bypassNum){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    private val issueIn = issueNode.in.head._1
    private val issueAlu = issueNode.out.filter(_._2._2.exuType == ExuType.alu).head._1
    private val issueJmp = issueNode.out.filter(_._2._2.exuType == ExuType.jmp).head._1
    val io = IO(new Bundle {
      val fenceio = new FenceIO
      val csrio = new CSRFileIO
      val issueToMou = Decoupled(new ExuInput)
      val writebackFromMou = Flipped(Decoupled(new ExuOutput))
    })

    issueAlu <> issueIn
    alu.module.io.bypassIn := bypassIn
    alu.module.redirectIn := redirectIn

    issueJmp <> issueIn
    jmp.module.io.bypassIn := bypassIn
    jmp.module.redirectIn := redirectIn

    jmp.module.io.fenceio <> io.fenceio
    jmp.module.io.csrio <> io.csrio
    io.issueToMou <> jmp.module.io.issueToMou
    io.writebackFromMou <> jmp.module.io.writebackFromMou

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, issueJmp.issue.ready)
    private val issueFuHit = issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_ | _)
    assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
