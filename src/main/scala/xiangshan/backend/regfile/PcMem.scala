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
package xiangshan.backend.regfile

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.frontend.Ftq_RF_Components
import xiangshan.{XSBundle, XSModule}

class PcWritePort(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(log2Ceil(FtqSize).W))
  val data = Input(new Ftq_RF_Components)
  val en = Input(Bool())
}

class PcReadPort(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(log2Ceil(FtqSize).W))
  val data = Output(new Ftq_RF_Components)
}

class PcMem(numRead:Int, numWrite:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val read = Vec(numRead, new PcReadPort)
    val write = Vec(numWrite, new PcWritePort)
  })
  private val dataWidth = (new Ftq_RF_Components).getWidth
  private val mem = Mem(FtqSize, UInt(dataWidth.W))
  io.write.foreach(w => {
    when(w.en){
      mem(w.addr) := w.data.asTypeOf(UInt(dataWidth.W))
    }
  })

  io.read.foreach(r => {
    r.data := mem(r.addr).asTypeOf(new Ftq_RF_Components)
  })
}
