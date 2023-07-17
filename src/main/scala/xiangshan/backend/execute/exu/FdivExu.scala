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
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.backend.execute.fu.fpu.FDivSqrt
import xiangshan.{ExuOutput, HasXSParameter, MicroOp}
import xs.utils.PickOneHigh

class FdivExu(id:Int, complexName:String)(implicit p:Parameters) extends BasicExu with HasXSParameter{
  private val cfg = ExuConfig(
    name = "FdivExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.fdivSqrtCfg, FuConfigs.fdivSqrtCfg, FuConfigs.fdivSqrtCfg),
    exuType = ExuType.fdiv,
    needToken = true,
    speculativeWakeup = true
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)
  lazy val module = new FdivExuImpl(this, cfg)
}
class FdivExuImpl(outer:FdivExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer) with HasXSParameter{
  val csr_frm: UInt = IO(Input(UInt(3.W)))
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  private val fdivSqrts = Seq.fill(exuCfg.fuConfigs.length)(Module(new FDivSqrt))
  private val outputArbiter = Module(new Arbiter(new MicroOp, exuCfg.fuConfigs.length))

  private val fuSel = PickOneHigh(Cat(fdivSqrts.map(_.io.in.ready).reverse))
  issuePort.issue.ready := true.B
  fdivSqrts.zipWithIndex.zip(outputArbiter.io.in).foreach({case((fu,idx), arbIn) =>
    fu.io.redirectIn := redirectIn
    fu.io.in.valid := issuePort.issue.valid &&
      fuSel.bits(idx) &&
      issuePort.issue.bits.uop.ctrl.fuType === exuCfg.fuConfigs.head.fuType &&
      !issuePort.issue.bits.uop.robIdx.needFlush(redirectIn)
    fu.io.in.bits.uop := issuePort.issue.bits.uop
    fu.io.in.bits.src := issuePort.issue.bits.src
    fu.rm := Mux(issuePort.issue.bits.uop.ctrl.fpu.rm =/= 7.U, issuePort.issue.bits.uop.ctrl.fpu.rm, csr_frm)
    fu.io.out.ready := arbIn.ready
    arbIn.valid := fu.io.out.valid
    arbIn.bits := fu.io.out.bits.uop
  })
  when(issuePort.issue.valid && issuePort.issue.bits.uop.ctrl.fuType === exuCfg.fuConfigs.head.fuType){assert(fuSel.valid)}
  writebackPort.valid := outputArbiter.io.out.valid && !outputArbiter.io.out.bits.robIdx.needFlush(redirectIn)
  writebackPort.bits.uop := outputArbiter.io.out.bits
  private val uopSel = RegEnable(UIntToOH(outputArbiter.io.chosen)(exuCfg.fuConfigs.length - 1, 0), outputArbiter.io.out.fire)
  private val dataOut = fdivSqrts.map(_.io.out.bits.data)
  private val fFlagOut = fdivSqrts.map(_.fflags)
  writebackPort.bits.data := Mux1H(uopSel, dataOut)
  writebackPort.bits.fflags := Mux1H(uopSel, fFlagOut)
  writebackPort.bits.redirect := DontCare
  writebackPort.bits.redirectValid := false.B
  writebackPort.bits.debug := DontCare
  outputArbiter.io.out.ready := true.B
}
