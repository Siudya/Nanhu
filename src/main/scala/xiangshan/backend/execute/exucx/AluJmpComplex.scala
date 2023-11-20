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
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.backend.execute.exu.{AluExu, ExuType, FenceIO, MiscExu}
import xiangshan.backend.execute.fu.csr.CSRFileIO
import xiangshan.{ExuInput, ExuOutput, FuType, XSCoreParamsKey}

class AluMiscComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluMiscComplex", bypassNum))
  val misc = LazyModule(new MiscExu(id, "AluMiscComplex", bypassNum))
  alu.issueNode :*= issueNode
  misc.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* misc.writebackNode
  lazy val module = new AluMiscComplexImp(this, id, bypassNum)
}
class AluMiscComplexImp(outer:AluMiscComplex, id: Int, bypassNum:Int) extends BasicExuComplexImp(outer, bypassNum) {
  require(outer.issueNode.in.length == 1)
  require(outer.issueNode.out.length == 2)
  private val issueIn = outer.issueNode.in.head._1
  private val issueAlu = outer.issueNode.out.filter(_._2._2.exuType == ExuType.alu).head._1
  private val issueMisc = outer.issueNode.out.filter(_._2._2.exuType == ExuType.misc).head._1
  val io = IO(new Bundle {
    val fenceio = new FenceIO
    val csrio = new CSRFileIO
    val issueToMou = Decoupled(new ExuInput)
    val writebackFromMou = Flipped(Decoupled(new ExuOutput))
  })

  issueAlu <> issueIn
  outer.alu.module.io.bypassIn := bypassIn
  outer.alu.module.redirectIn := redirectIn

  issueMisc <> issueIn
  outer.misc.module.io.bypassIn := bypassIn
  outer.misc.module.redirectIn := redirectIn

  outer.misc.module.io.fenceio <> io.fenceio
  outer.misc.module.io.csrio <> io.csrio
  io.issueToMou <> outer.misc.module.io.issueToMou
  io.writebackFromMou <> outer.misc.module.io.writebackFromMou

  issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, issueMisc.issue.ready)
  private val issueFuHit = outer.issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_ | _)
  when(issueIn.issue.valid) {
    assert(issueFuHit)
  }
}