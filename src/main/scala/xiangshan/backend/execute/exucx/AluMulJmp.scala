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
import xiangshan.backend.execute.exu.{ExuType, AluExu, MulExu, JmpExu}
import xiangshan._
import chisel3.util._
import xiangshan.{DistributedCSRIO, XSCoreParamsKey}
import xiangshan.backend.execute.fu.FDICallJumpExcpIO

class AluMulJmpComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluMulJmpComplex", bypassNum))
  val mul = LazyModule(new MulExu(id, "AluMulJmpComplex", bypassNum))
  val jmp = LazyModule(new JmpExu(id, "AluMulJmpComplex", bypassNum))
  

  alu.issueNode :*= issueNode
  mul.issueNode :*= issueNode
  jmp.issueNode :*= issueNode

  writebackNode :=* alu.writebackNode
  writebackNode :=* mul.writebackNode
  writebackNode :=* jmp.writebackNode

  lazy val module = new AluMulJmpComplexImp(this, id, bypassNum)
}

class AluMulJmpComplexImp(outer:AluMulJmpComplex, id:Int, bypassNum:Int) extends BasicExuComplexImp(outer, bypassNum){
    require(outer.issueNode.in.length == 1)
    require(outer.issueNode.out.length == 3)
    private val issueIn = outer.issueNode.in.head._1
    private val issueAlu = outer.issueNode.out.filter(_._2._2.exuType == ExuType.alu).head._1
    private val issueMul = outer.issueNode.out.filter(_._2._2.exuType == ExuType.mul).head._1
    private val issueJmp = outer.issueNode.out.filter(_._2._2.exuType == ExuType.jmp).head._1

    val io = IO(new Bundle {
      val bypassOut = Output(Valid(new ExuOutput))
      val csr_frm: UInt = Input(UInt(3.W))
      val prefetchI = Output(Valid(UInt(p(XSCoreParamsKey).XLEN.W)))
      val fdicallJumpExcpIO = Output(new FDICallJumpExcpIO())
      val fdicallDistributedCSR = Input(new DistributedCSRIO())
    })

    issueAlu <> issueIn
    outer.alu.module.io.bypassIn := bypassIn
    outer.alu.module.redirectIn := redirectIn

    issueMul <> issueIn
    outer.mul.module.io.bypassIn := bypassIn
    outer.mul.module.io.csr_frm := RegNext(io.csr_frm)
    outer.mul.module.redirectIn := redirectIn
    io.bypassOut := outer.mul.module.io.bypassOut

    issueJmp <> issueIn
    outer.jmp.module.io.bypassIn := bypassIn
    outer.jmp.module.redirectIn := redirectIn
    io.prefetchI := outer.jmp.module.io.prefetchI

    outer.jmp.module.io.fdicallJumpExcpIO <> io.fdicallJumpExcpIO
    outer.jmp.module.io.fdicallDistributedCSR <> io.fdicallDistributedCSR

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, 
                                Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.jmp, issueJmp.issue.ready, issueMul.issue.ready))

    private val issueFuHit = outer.issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_|_)
    when(issueIn.issue.valid){assert(issueFuHit)}
  }