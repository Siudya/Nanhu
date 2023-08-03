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

import chisel3.util.Pipe
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import xiangshan.backend.execute.exucx.{FmaDivComplex, FmaMiscComplex, FmacComplex}
import freechips.rocketchip.diplomacy.LazyModule

class FloatingBlock(implicit p:Parameters) extends BasicExuBlock{
  private val fmacs = Seq.tabulate(fmaNum)(idx => LazyModule(new FmacComplex(idx)))
  private val fmacDivs = Seq.tabulate(fmaDivNum)(idx => LazyModule(new FmaDivComplex(idx)))
  private val fmaMiscs = Seq.tabulate(fmaMiscNum)(idx => LazyModule(new FmaMiscComplex(idx)))
  private val fpComplexes = fmacs ++ fmacDivs ++ fmaMiscs
  fpComplexes.foreach(exucx => {
    exucx.issueNode :*= issueNode
    writebackNode :=* exucx.writebackNode
  })
  lazy val module = new BasicExuBlockImp(this){
    val io = IO(new Bundle{
      val csr_frm: UInt = Input(UInt(3.W))
    })
    fpComplexes.foreach(_.module.redirectIn := Pipe(redirectIn))
    fmacs.foreach(_.module.csr_frm := RegNext(io.csr_frm))
    fmacDivs.foreach(_.module.csr_frm := RegNext(io.csr_frm))
    fmaMiscs.foreach(_.module.csr_frm := RegNext(io.csr_frm))
  }
}
