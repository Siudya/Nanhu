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

package top

import chisel3._
import org.chipsalliance.cde.config
import device._
import freechips.rocketchip.amba.axi4.{AXI4EdgeParameters, AXI4MasterNode, AXI4SlaveNode, AXI4Xbar}
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}
import difftest._

class SimMMIO(edge: AXI4EdgeParameters, dmaEdge: AXI4EdgeParameters)(implicit p: config.Parameters) extends LazyModule {

  val node = AXI4MasterNode(List(edge.master))
  val dma_node = AXI4SlaveNode(List(dmaEdge.slave))

  val flash = LazyModule(new AXI4Flash(Seq(AddressSet(0x10000000L, 0xfffffff))))
  val uart = LazyModule(new AXI4UART(Seq(AddressSet(0x40600000L, 0xf))))
  val dmaGen = LazyModule(new AXI4FakeDMA(Seq(AddressSet(0x37030000L, 0x0000ffffL)), dmaEdge.master))

  val axiBus = AXI4Xbar()

  uart.node := axiBus
  // vga.node :*= axiBus
  flash.node := axiBus
  dmaGen.node := axiBus

  axiBus := node
  dma_node := dmaGen.dma_node

  val io_axi4 = InModuleBody {
    node.makeIOs()
  }

  val io_dma = InModuleBody {
    dma_node.makeIOs()
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle() {
      val uart = new UARTIO
      val interrupt = new IntrGenIO
    })
    io.uart <> uart.module.io.extra.get
    io.interrupt := 0.U.asTypeOf(io.interrupt)
  }
}
