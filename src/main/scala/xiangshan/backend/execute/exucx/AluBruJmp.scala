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
import xiangshan.backend.execute.exu.{ExuType, AluExu, BruExu, JmpExu}
import xiangshan._
import chisel3.util._
import xiangshan.{DistributedCSRIO, XSCoreParamsKey}
import xiangshan.backend.execute.fu.FDICallJumpExcpIO

class AluBruJmpComplex(id: Int, bypassNum: Int)(implicit p: Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluBruJmpComplex", bypassNum))
  val bru = LazyModule(new BruExu(id, "AluBruJmpComplex", bypassNum))
  val jmp = LazyModule(new JmpExu(id, "AluBruJmpComplex", bypassNum))
  

  alu.issueNode :*= issueNode
  bru.issueNode :*= issueNode
  jmp.issueNode :*= issueNode

  writebackNode :=* alu.writebackNode
  writebackNode :=* bru.writebackNode
  writebackNode :=* jmp.writebackNode

  lazy val module = new AluBruJmpComplexImp(this, id, bypassNum)
}

class AluBruJmpComplexImp(outer:AluBruJmpComplex, id:Int, bypassNum:Int) extends BasicExuComplexImp(outer, bypassNum){
    require(outer.issueNode.in.length == 1)
    require(outer.issueNode.out.length == 3)
    private val issueIn = outer.issueNode.in.head._1
    private val issueAlu = outer.issueNode.out.filter(_._2._2.exuType == ExuType.alu).head._1
    private val issueBru = outer.issueNode.out.filter(_._2._2.exuType == ExuType.bru).head._1
    private val issueJmp = outer.issueNode.out.filter(_._2._2.exuType == ExuType.jmp).head._1

    val io = IO(new Bundle {
      val prefetchI = Output(Valid(UInt(p(XSCoreParamsKey).XLEN.W)))
      val fdicallJumpExcpIO = Output(new FDICallJumpExcpIO())
      val fdicallDistributedCSR = Input(new DistributedCSRIO())
    })

    issueAlu <> issueIn
    outer.alu.module.io.bypassIn := bypassIn
    outer.alu.module.redirectIn := redirectIn

    issueBru <> issueIn
    outer.bru.module.io.bypassIn  := bypassIn
    outer.bru.module.redirectIn   := redirectIn

    issueJmp <> issueIn
    outer.jmp.module.io.bypassIn := bypassIn
    outer.jmp.module.redirectIn := redirectIn
    io.prefetchI := outer.jmp.module.io.prefetchI

    outer.jmp.module.io.fdicallJumpExcpIO <> io.fdicallJumpExcpIO
    outer.jmp.module.io.fdicallDistributedCSR <> io.fdicallDistributedCSR

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, 
                                Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.jmp, issueJmp.issue.ready, issueBru.issue.ready))

    private val issueFuHit = outer.issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_|_)
    when(issueIn.issue.valid){assert(issueFuHit)}
  }