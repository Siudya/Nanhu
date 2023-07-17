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
import chisel3.util.Valid
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.backend.execute.exu.{AluExu, ExuType, MulExu}
import xiangshan.{ExuOutput, FuType}

class AluMulComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluMulComplex", bypassNum + 1))
  val mul = LazyModule(new MulExu(id, "AluMulComplex", bypassNum))
  alu.issueNode :*= issueNode
  mul.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* mul.writebackNode

  lazy val module = new BasicExuComplexImp(this, bypassNum){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    val io = IO(new Bundle{
      val bypassOut = Output(Valid(new ExuOutput))
    })
    private val issueIn = issueNode.in.head._1
    private val issueAlu = issueNode.out.filter(_._2._2.exuType == ExuType.alu).head._1
    private val issueMul = issueNode.out.filter(_._2._2.exuType == ExuType.mul).head._1

    issueAlu <> issueIn
    alu.module.io.bypassIn.take(bypassNum).zip(bypassIn).foreach({case(a, b) => a := b})
    alu.module.redirectIn := redirectIn

    issueMul <> issueIn
    mul.module.io.bypassIn := bypassIn
    mul.module.redirectIn := redirectIn

    alu.module.io.bypassIn.last := mul.module.io.bypassOut
    io.bypassOut := mul.module.io.bypassOut

    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, issueMul.issue.ready)
    private val issueFuHit = issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_ | _)
    when(issueIn.issue.valid){assert(issueFuHit)}
  }
}
