/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
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
import xiangshan.XSModule
class PayloadArrayReadIO[T <: Data](gen:T, entryNum:Int) extends Bundle {
  val addr = Input(UInt(entryNum.W))
  val data = Output(gen)
}

class PayloadArrayWriteIO[T <: Data](gen:T, entryNum:Int) extends Bundle {
  val en: Bool = Input(Bool())
  val addr: UInt = Input(UInt(entryNum.W))
  val data: T = Input(gen)
}

class PayloadArray[T <: Data](gen:T, entryNum:Int, deqNum:Int, name:String)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val read = Vec(deqNum, new PayloadArrayReadIO(gen, entryNum))
    val write = new PayloadArrayWriteIO(gen, entryNum)
  })
  override val desiredName:String = name

  private val payload = Reg(Vec(entryNum, gen))

  // read ports
  io.read.foreach(r => {r.data := Mux1H(r.addr.asBools, payload)})

  // write ports
  for (idx <- 0 until entryNum) {
    val wen = io.write.en & io.write.addr(idx)
    val data = io.write.data
    when(wen){
      payload(idx) := data
    }
  }

  io.read.foreach(r => assert(PopCount(r.addr) === 1.U))
  when(io.write.en){
    assert(PopCount(io.write.addr) === 1.U)
  }
}