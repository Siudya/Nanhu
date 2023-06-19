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
package xiangshan.backend.issue
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xs.utils.ParallelOperation

sealed class SelectArbiterInternalBundle(bankNum:Int, entryNum:Int, inNum:Int)(implicit p: Parameters) extends Bundle {
  val valid = Bool()
  val info = new SelectResp(bankNum, entryNum)
  val idxOH = UInt(inNum.W)
}

object SelectRespArbiter{
  def select2(in0:SelectArbiterInternalBundle, in1:SelectArbiterInternalBundle):SelectArbiterInternalBundle = {
    val valid0 = in0.valid
    val valid1 = in1.valid
    val ptr0 = in0.info.info.robPtr
    val ptr1 = in1.info.info.robPtr
    val validVec = Cat(valid1, valid0)
    val sel = WireInit(true.B)
    switch(validVec){
      is("b01".U){
        sel := true.B
      }
      is("b10".U){
        sel := false.B
      }
      is("b11".U){
        sel := ptr0 < ptr1
      }
    }
    val res = Mux(sel, in0, in1)
    res
  }
}

class SelectRespArbiter(bankNum:Int, entryNum:Int, inNum:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in = Vec(inNum, Flipped(Decoupled(new SelectResp(bankNum, entryNum))))
    val out = Decoupled(new SelectResp(bankNum, entryNum))
    val chosen = Output(UInt(inNum.W))
  })
  private val infoSeq = Seq.tabulate(inNum)(idx =>{
    val res = Wire(new SelectArbiterInternalBundle(bankNum, entryNum, inNum))
    res.valid := io.in(idx).valid
    res.info := io.in(idx).bits
    res.idxOH := (1 << idx).U
    res
  })
  private val res = ParallelOperation(infoSeq, SelectRespArbiter.select2)
  io.out.valid := res.valid
  io.out.bits := res.info
  io.chosen := res.idxOH

  io.in.zip(res.idxOH.asBools).foreach({case(in, sel) =>
    in.ready := sel && io.out.ready
  })
}
