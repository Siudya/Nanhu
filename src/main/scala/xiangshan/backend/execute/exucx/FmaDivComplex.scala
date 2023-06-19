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
package xiangshan.backend.execute.exucx

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.FuType
import xiangshan.backend.execute.exu.{ExuType, FdivExu, FmacExu}

class FmaDivComplex (id: Int)(implicit p:Parameters) extends BasicExuComplex{
  val fmac = LazyModule(new FmacExu(id,"FmaDivComplex"))
  val fdiv = LazyModule(new FdivExu(id, "FmaDivComplex"))
  fmac.issueNode :*= issueNode
  fdiv.issueNode :*= issueNode
  writebackNode :=* fmac.writebackNode
  writebackNode :=* fdiv.writebackNode
  lazy val module = new BasicExuComplexImp(this, 0) {
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    val csr_frm: UInt = IO(Input(UInt(3.W)))
    private val issueIn = issueNode.in.head._1
    private val issueFmac = issueNode.out.filter(_._2._2.exuType == ExuType.fmac).head._1
    private val issueFdiv = issueNode.out.filter(_._2._2.exuType == ExuType.fdiv).head._1

    issueFmac <> issueIn
    fmac.module.redirectIn := redirectIn
    fmac.module.csr_frm := csr_frm

    issueFdiv <> issueIn
    fdiv.module.redirectIn := redirectIn
    fdiv.module.csr_frm := csr_frm

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.fmac, issueFmac.issue.ready, issueFmac.issue.ready)
    private val issueFuHit = issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_ | _)
    assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
