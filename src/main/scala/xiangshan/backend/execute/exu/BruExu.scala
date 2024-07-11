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

package xiangshan.backend.execute.exu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.ExuOutput
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.backend.execute.fu.bru.BranchUnit

class BruExu(id: Int, complexName: String, val bypassInNum: Int)(implicit p: Parameters) extends BasicExu {
  private val cfg  = ExuConfig(
    name = "BruExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.bruCfg),
    exuType = ExuType.bru,
    writebackToRob = true,
    writebackToVms = false
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)

  lazy val module = new BruExuImpl(this, cfg)
}

class BruExuImpl(outer: BruExu, exuCfg: ExuConfig)(implicit p: Parameters) extends BasicExuImpl(outer) {
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  issuePort.issue.ready := true.B
  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)

  private val bru = Module(new BranchUnit)
  bru.io.redirectIn := redirectIn
  bru.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === exuCfg.fuConfigs.head.fuType
  bru.io.in.bits.uop := finalIssueSignals.bits.uop
  bru.io.in.bits.src := finalIssueSignals.bits.src
  bru.io.out.ready := true.B
  assert(bru.io.in.ready)

  writebackPort := DontCare
  writebackPort.valid := bru.io.out.valid
  writebackPort.bits.wakeupValid := true.B
  writebackPort.bits.uop := bru.io.out.bits.uop
  writebackPort.bits.data := bru.io.out.bits.data
  writebackPort.bits.redirectValid := bru.redirectOutValid
  writebackPort.bits.redirect := bru.redirectOut
}
