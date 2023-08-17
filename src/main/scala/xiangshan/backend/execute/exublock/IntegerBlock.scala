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

import chipsalliance.rocketchip.config.Parameters
import xiangshan.backend.execute.exucx.{AluDivComplex, AluJmpComplex, AluMulComplex}
import freechips.rocketchip.diplomacy.LazyModule
import chisel3._
import chisel3.util._
import xiangshan.{ExuInput, ExuOutput, XSCoreParamsKey}
import xiangshan.backend.execute.exu.FenceIO
import xiangshan.backend.execute.fu.csr.CSRFileIO

class IntegerBlock(implicit p:Parameters) extends BasicExuBlock {
  require(jmpNum == 1)
  private val aluMuls = Seq.tabulate(mulNum)(idx => LazyModule(new AluMulComplex(idx, 0)))
  private val aluDivs = Seq.tabulate(divNum)(idx => LazyModule(new AluDivComplex(idx, 0)))
  private val aluJmps = Seq.tabulate(jmpNum)(idx => LazyModule(new AluJmpComplex(idx, 0)))
  private val intComplexes = aluMuls ++ aluDivs ++ aluJmps
  intComplexes.foreach(exucx => {
    exucx.issueNode :*= issueNode
    writebackNode :=* exucx.writebackNode
  })

  lazy val module = new BasicExuBlockImp(this){
    val io = IO(new Bundle {
      val fenceio = new FenceIO
      val csrio = new CSRFileIO
      val issueToMou = Decoupled(new ExuInput)
      val writebackFromMou = Flipped(Decoupled(new ExuOutput))
      val prefetchI = Output(Valid(UInt(p(XSCoreParamsKey).XLEN.W)))
    })
    intComplexes.foreach(_.module.redirectIn := Pipe(redirectIn))

    (aluJmps ++ aluDivs ++ aluMuls).foreach(cplx => cplx.module.bypassIn.zip(aluMuls.map(_.module.io.bypassOut)).foreach({ case (a, b) => a := b }))

    aluJmps.head.module.io.fenceio <> io.fenceio
    aluJmps.head.module.io.fenceio.sbuffer.sbIsEmpty := io.fenceio.sbuffer.sbIsEmpty
    aluJmps.head.module.io.csrio <> io.csrio
    aluJmps.head.module.io.issueToMou <> io.issueToMou
    aluJmps.head.module.io.writebackFromMou <> io.writebackFromMou
    io.prefetchI := aluJmps.head.module.io.prefetchI
  }
}
