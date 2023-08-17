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
import xiangshan.backend.execute.fu.bku.Bku
import xiangshan.backend.execute.fu.fpu.IntToFP
import xiangshan.backend.execute.fu.mdu.{ArrayMultiplier, MDUOpType}
import xiangshan.{ExuOutput, HasXSParameter}
import xs.utils.{LookupTree, ParallelMux, SignExt, ZeroExt}

class MulExu(id:Int, complexName:String, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu{
  private val cfg  = ExuConfig(
    name = "MulExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.mulCfg, FuConfigs.bkuCfg, FuConfigs.i2fCfg),
    exuType = ExuType.mul
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)

  lazy val module = new MulExuImpl(this, cfg)
}
class MulExuImpl(outer:MulExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer) with HasXSParameter{
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
    val bypassOut = Output(Valid(new ExuOutput))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  private val mul = Module(new ArrayMultiplier(XLEN + 1)(p))
  private val bku = Module(new Bku)
  private val i2f = Module(new IntToFP)

  issuePort.issue.ready := true.B
  private val finalIssueSignals = bypassSigGen(io.bypassIn :+ io.bypassOut, issuePort, outer.bypassInNum > 0)
  private val fuSeq = Seq(mul, bku, i2f)
  fuSeq.zip(exuCfg.fuConfigs).foreach({case(m, cfg) =>
    m.io.redirectIn := redirectIn
    m.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === cfg.fuType
    m.io.in.bits.uop := finalIssueSignals.bits.uop
    m.io.in.bits.src := finalIssueSignals.bits.src
    m.io.out.ready := true.B
    when(m.io.in.valid){assert(m.io.in.ready)}
  })
  i2f.rm := finalIssueSignals.bits.uop.ctrl.fpu.rm

  private val (func, src1, src2) = (
    finalIssueSignals.bits.uop.ctrl.fuOpType,
    finalIssueSignals.bits.src(0)(XLEN - 1, 0),
    finalIssueSignals.bits.src(1)(XLEN - 1, 0)
  )
  private val op = MDUOpType.getMulOp(func)
  private val signext = SignExt(_: UInt, XLEN + 1)
  private val zeroext = ZeroExt(_: UInt, XLEN + 1)
  private val mulInputFuncTable = List(
    MDUOpType.mul -> (zeroext, zeroext),
    MDUOpType.mulh -> (signext, signext),
    MDUOpType.mulhsu -> (signext, zeroext),
    MDUOpType.mulhu -> (zeroext, zeroext)
  )

  mul.io.in.bits.src(0) := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1, 0), p._2._1(src1)))
  )
  when(func(3)) {
    mul.io.in.bits.src(0) := src1(6, 0)
  }
  mul.io.in.bits.src(1) := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1, 0), p._2._2(src2)))
  )

  private val isW = MDUOpType.isW(func)
  private val isH = MDUOpType.isH(func)
  mul.ctrl.isW := isW
  mul.ctrl.isHi := isH
  mul.ctrl.sign := DontCare

  private val outValids = fuSeq.map(m => RegNext(m.io.out.valid, false.B))
  private val outUops = fuSeq.map(m => RegEnable(m.io.out.bits.uop, m.io.out.valid))
  private val outData = fuSeq.map(m => m.io.out.bits.data)
  private val finalValid = outValids.reduce(_|_)
  private val finalUop = Mux1H(outValids, outUops)
  private val finalData = Mux1H(outValids, outData)
  writebackPort := DontCare
  writebackPort.valid := finalValid && !finalUop.robIdx.needFlush(redirectIn)
  writebackPort.bits.uop := finalUop
  writebackPort.bits.data := finalData
  writebackPort.bits.fflags := i2f.fflags
  io.bypassOut.valid := writebackPort.valid && writebackPort.bits.uop.ctrl.rfWen && writebackPort.bits.uop.pdest =/= 0.U
  io.bypassOut.bits := writebackPort.bits

  assert(mul.io.in.ready)
  assert(bku.io.in.ready)
  assert(i2f.io.in.ready)
  assert(PopCount(outValids) <= 1.U)
}
