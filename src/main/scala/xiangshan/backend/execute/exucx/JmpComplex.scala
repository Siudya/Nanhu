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
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.backend.execute.exu.{JmpExu, ExuType}
import xiangshan.XSCoreParamsKey

class JmpComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val jmp = LazyModule(new JmpExu(id, "JmpComplex", bypassNum))
  jmp.issueNode :*= issueNode
  writebackNode :=* jmp.writebackNode
  lazy val module = new JmpComplexImp(this, id, bypassNum)
}
class JmpComplexImp(outer:JmpComplex, id: Int, bypassNum:Int) extends BasicExuComplexImp(outer, bypassNum) {
  require(outer.issueNode.in.length == 1)
  private val issueIn = outer.issueNode.in.head._1
  private val issueOut = outer.issueNode.out.head._1
  val io = IO(new Bundle {
    val prefetchI = Output(Valid(UInt(p(XSCoreParamsKey).XLEN.W)))
  })

  issueOut <> issueIn
  outer.jmp.module.io.bypassIn := bypassIn
  outer.jmp.module.redirectIn := redirectIn
  io.prefetchI := outer.jmp.module.io.prefetchI

  issueIn.issue.ready := issueOut.issue.ready
}