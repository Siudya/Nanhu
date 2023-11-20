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
import xiangshan.backend.execute.fu.csr.{CSR, CSRFileIO}
import xiangshan.backend.execute.fu.fence.{SfenceBundle, _}
import xiangshan.backend.execute.fu.jmp._
import xiangshan.backend.execute.fu.{FUWithRedirect, FuConfigs, FunctionUnit}
import xiangshan._
import xs.utils.{DelayN, ParallelMux}
class JmpExu (id:Int, complexName:String, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu{
  private val cfg = ExuConfig(
    name = "JmpExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.jmpCfg),
    exuType = ExuType.jmp,
    writebackToRob = true,
    writebackToVms = false
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)
  override lazy val module = new JmpExuImpl(this, cfg)
}

class JmpExuImpl(outer:JmpExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer) {
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
    val prefetchI = Output(Valid(UInt(p(XSCoreParamsKey).XLEN.W)))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1
  private val jmp = Module(new Jump)
  issuePort.issue.ready := true.B

  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)

  jmp.io.redirectIn := redirectIn
  jmp.io.in.valid := finalIssueSignals.valid
  jmp.io.in.bits.uop := finalIssueSignals.bits.uop
  jmp.io.in.bits.src := finalIssueSignals.bits.src
  jmp.io.out.ready := true.B

  io.prefetchI := Pipe(jmp.prefetchI)
  writebackPort := DontCare
  writebackPort.valid := jmp.io.out.valid
  writebackPort.bits.wakeupValid := true.B
  writebackPort.bits.uop := jmp.io.out.bits.uop
  writebackPort.bits.data := jmp.io.out.bits.data

  writebackPort.bits.fflags := DontCare
  writebackPort.bits.redirect := jmp.redirectOut
  writebackPort.bits.redirectValid := jmp.redirectOutValid
  writebackPort.bits.debug.isMMIO := false.B
  writebackPort.bits.debug.isPerfCnt := false.B
  writebackPort.bits.debug.paddr := 0.U
  writebackPort.bits.debug.vaddr := 0.U
}
