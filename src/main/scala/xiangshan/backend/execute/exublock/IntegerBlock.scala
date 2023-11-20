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
package xiangshan.backend.execute.exublock

import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.execute.exucx.{AluDivComplex, AluMiscComplex, AluMulComplex, JmpComplex}
import freechips.rocketchip.diplomacy.LazyModule
import chisel3._
import chisel3.util._
import xiangshan.{ExuInput, ExuOutput, XSCoreParamsKey}
import xiangshan.backend.execute.exu.FenceIO
import xiangshan.backend.execute.fu.csr.CSRFileIO

class IntegerBlock(implicit p:Parameters) extends BasicExuBlock {
  require(miscNum == 1)
  val aluMuls = Seq.tabulate(mulNum)(idx => LazyModule(new AluMulComplex(idx, 0)))
  val aluDivs = Seq.tabulate(divNum)(idx => LazyModule(new AluDivComplex(idx, 0)))
  val aluMiscs = Seq.tabulate(miscNum)(idx => LazyModule(new AluMiscComplex(idx, 0)))
  val jmps = Seq.tabulate(jmpNum)(idx => LazyModule(new JmpComplex(idx, 0)))
  val intComplexes = aluMuls ++ aluDivs ++ aluMiscs ++ jmps
  intComplexes.foreach(exucx => {
    exucx.issueNode :*= issueNode
    writebackNode :=* exucx.writebackNode
  })

  lazy val module = new IntegerBlockImp(this)
}
class IntegerBlockImp(outer:IntegerBlock) extends BasicExuBlockImp(outer){
  val io = IO(new Bundle {
    val fenceio = new FenceIO
    val csrio = new CSRFileIO
    val issueToMou = Decoupled(new ExuInput)
    val writebackFromMou = Flipped(Decoupled(new ExuOutput))
    val prefetchI = Output(Valid(UInt(p(XSCoreParamsKey).XLEN.W)))
  })
  outer.intComplexes.foreach(_.module.redirectIn := Pipe(redirectIn))

  (outer.aluMiscs ++ outer.aluDivs ++ outer.aluMuls ++ outer.jmps)
    .foreach(cplx => cplx.module.bypassIn.zip(outer.aluMuls.map(_.module.io.bypassOut))
      .foreach({ case (a, b) => a := b }))

  outer.aluMiscs.head.module.io.fenceio <> io.fenceio
  outer.aluMiscs.head.module.io.fenceio.sbuffer.sbIsEmpty := io.fenceio.sbuffer.sbIsEmpty
  outer.aluMiscs.head.module.io.csrio <> io.csrio
  outer.aluMiscs.head.module.io.issueToMou <> io.issueToMou
  outer.aluMiscs.head.module.io.writebackFromMou <> io.writebackFromMou
  outer.aluMuls.foreach(_.module.io.csr_frm := io.csrio.fpu.frm)
  io.prefetchI := outer.jmps.head.module.io.prefetchI
}
