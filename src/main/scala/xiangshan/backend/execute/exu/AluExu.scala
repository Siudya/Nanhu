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
import xiangshan.ExuOutput
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.backend.execute.fu.alu.Alu

class AluExu(id:Int, complexName:String, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu{
  private val cfg  = ExuConfig(
    name = "AluExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.aluCfg),
    exuType = ExuType.alu
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)

  lazy val module = new AluExuImpl(this, cfg)
}
class AluExuImpl(outer:AluExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer){
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput))) //Alu does not need bypass out for its latency is 0. Bypassing in regfile is enough.
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  issuePort.issue.ready := true.B
  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)

  private val alu = Module(new Alu)
  alu.io.redirectIn := redirectIn
  alu.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === exuCfg.fuConfigs.head.fuType
  alu.io.in.bits.uop := finalIssueSignals.bits.uop
  alu.io.in.bits.src := finalIssueSignals.bits.src
  alu.io.out.ready := true.B

  writebackPort := DontCare
  writebackPort.valid := alu.io.out.valid
  writebackPort.bits.uop := alu.io.out.bits.uop
  writebackPort.bits.data := alu.io.out.bits.data
  writebackPort.bits.redirectValid := alu.redirectOutValid
  writebackPort.bits.redirect := alu.redirectOut
  assert(alu.io.in.ready)
}
