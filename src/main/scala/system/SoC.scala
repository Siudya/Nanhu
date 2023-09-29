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

package system

import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import device.{DebugModule, DebugModuleIO, TLPMA, TLPMAIO}
import freechips.rocketchip.devices.tilelink.{CLINT, CLINTParams, DevNullParams, PLICParams, TLError, TLPLIC}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, InModuleBody, LazyModule, LazyModuleImp, MemoryDevice, RegionType, SimpleDevice, TransferSizes}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import xs.utils.tl.TLEdgeBuffer
import xiangshan.{HasXSParameter, XSBundle, XSCore, XSCoreParameters, XSTileKey}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import top.BusPerfMonitor
import huancun._
import xs.utils.tl.TLLogger
import xiangshan.backend.execute.fu.PMAConst
import axi2tl._
import xs.utils.perf.DebugOptionsKey

case object SoCParamsKey extends Field[SoCParameters]

case class SoCParameters
(
  EnableILA: Boolean = false,
  PAddrBits: Int = 37,
  extIntrs: Int = 256,
  L3NBanks: Int = 4,
  L3CacheParamsOpt: Option[HCCacheParameters] = Some(HCCacheParameters(
    name = "l3",
    ways = 8,
    sets = 2048 // 1MB per bank
  )),
  hasMbist:Boolean = false,
  hasShareBus:Boolean = false
){
  // L3 configurations
  val L3InnerBusWidth = 256
  val L3BlockSize = 64
  // on chip network configurations
  val L3OuterBusWidth = 256
}

trait HasSoCParameter {
  implicit val p: Parameters

  val soc = p(SoCParamsKey)
  val debugOpts = p(DebugOptionsKey)
  val tiles = p(XSTileKey)

  val NumCores = tiles.size
  val EnableILA = soc.EnableILA

  // L3 configurations
  val L3InnerBusWidth = soc.L3InnerBusWidth
  val L3BlockSize = soc.L3BlockSize
  val L3NBanks = soc.L3NBanks

  // on chip network configurations
  val L3OuterBusWidth = soc.L3OuterBusWidth

  val NrExtIntr = soc.extIntrs
}

class ILABundle extends Bundle {}


abstract class BaseSoC()(implicit p: Parameters) extends LazyModule with HasSoCParameter {
  val PAddrBits = p(SoCParamsKey).PAddrBits
  val bankedNode = BankBinder(L3NBanks, L3BlockSize)
  val peripheralXbar = TLXbar()
  val l3_xbar = TLXbar()
  val l3_banked_xbar = TLXbar()
}

// We adapt the following three traits from rocket-chip.
// Source: rocket-chip/src/main/scala/subsystem/Ports.scala
trait HaveSlaveAXI4Port {
  this: BaseSoC =>

  val idBits = 14

  val l3FrontendAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "dma",
      id = IdRange(0, 1 << idBits)
    ))
  )))
  private val errorDevice = LazyModule(new TLError(
    params = DevNullParams(
      address = Seq(AddressSet(0x0, 0x7fffffffL)),
      maxAtomic = 8,
      maxTransfer = 64),
    beatBytes = L3InnerBusWidth / 8
  ))
  private val error_xbar = TLXbar()

  l3_xbar :=
    TLBuffer() :=
    AXI2TL(16,16) :=
    AXI4Buffer() :=
    l3FrontendAXI4Node
  errorDevice.node := l3_xbar

  val dma = InModuleBody {
    l3FrontendAXI4Node.makeIOs()
  }
}

trait HaveAXI4MemPort {
  this: BaseSoC =>
  val device = new MemoryDevice

  val memAddrMask = (1L << PAddrBits) - 1L
  val memRange = AddressSet(0x00000000L, memAddrMask).subtract(AddressSet(0x00000000L, 0x7FFFFFFFL))
  val memAXI4SlaveNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = memRange,
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, L3BlockSize),
          supportsWrite = TransferSizes(1, L3BlockSize),
          interleavedId = Some(0),
          resources = device.reg("mem")
        )
      ),
      beatBytes = L3OuterBusWidth / 8
    )
  ))

  val mem_xbar = TLXbar()
  mem_xbar :=*
    TLXbar() :=*
    TLBuffer.chainNode(2) :=*
    TLCacheCork() :=*
    bankedNode

  memAXI4SlaveNode :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4IdIndexer(idBits = 14) :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(L3BlockSize) :=
    TLToAXI4() :=
    TLSourceShrinker(64) :=
    TLWidthWidget(L3OuterBusWidth / 8) :=
    TLBuffer.chainNode(2) :=
    mem_xbar

  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }
}

trait HaveAXI4PeripheralPort { this: BaseSoC =>
  // on-chip devices: 0x3800_0000 - 0x3fff_ffff 0x0000_0000 - 0x0000_0fff
  val onChipPeripheralRange = AddressSet(0x38000000L, 0x07ffffffL)
  val uartRange = AddressSet(0x40600000, 0xf)
  val uartDevice = new SimpleDevice("serial", Seq("xilinx,uartlite"))
  val uartParams = AXI4SlaveParameters(
    address = Seq(uartRange),
    regionType = RegionType.UNCACHED,
    supportsRead = TransferSizes(1, 8),
    supportsWrite = TransferSizes(1, 8),
    resources = uartDevice.reg
  )
  val periAddrMask = (1L << PAddrBits) - 1L
  val peripheralRange = AddressSet(0x00000000L, periAddrMask).subtract(onChipPeripheralRange).flatMap(x => x.subtract(uartRange))
  val peripheralNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address = peripheralRange,
      regionType = RegionType.UNCACHED,
      supportsRead = TransferSizes(1, 8),
      supportsWrite = TransferSizes(1, 8),
      interleavedId = Some(0)
    ), uartParams),
    beatBytes = 8
  )))

  peripheralNode :=
    AXI4IdIndexer(idBits = 4) :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(8) :=
    TLToAXI4() :=
    TLBuffer.chainNode(3) :=
    peripheralXbar

  val peripheral = InModuleBody {
    peripheralNode.makeIOs()
  }

}

class SoCMisc()(implicit p: Parameters) extends BaseSoC
  with HaveAXI4MemPort
  with HaveAXI4PeripheralPort
  with PMAConst
  with HaveSlaveAXI4Port
{
  val peripheral_ports = Array.fill(NumCores) { TLTempNode() }
  val core_to_l3_ports = Array.fill(NumCores) { TLTempNode() }

  val l3_in = TLTempNode()
  val l3_out = TLTempNode()
  val l3_mem_pmu = BusPerfMonitor(enable = !debugOpts.FPGAPlatform)

  l3_in :*= TLEdgeBuffer(_ => true, Some("L3_in_buffer")) :*= l3_banked_xbar
  bankedNode :*= TLLogger("MEM_L3", !debugOpts.FPGAPlatform) :*= l3_mem_pmu :*= l3_out

  if(soc.L3CacheParamsOpt.isEmpty){
    l3_out :*= l3_in
  }

  for(port <- peripheral_ports) {
    peripheralXbar := TLBuffer.chainNode(2, Some("L2_to_L3_peripheral_buffer")) := port
  }

  for ((core_out, i) <- core_to_l3_ports.zipWithIndex){
    l3_banked_xbar :=*
      TLLogger(s"L3_L2_$i", !debugOpts.FPGAPlatform) :=*
      TLBuffer() :=
      core_out
  }
  l3_banked_xbar := TLBuffer.chainNode(2) := l3_xbar

  val clint = LazyModule(new CLINT(CLINTParams(0x38000000L), 8))
  clint.node := peripheralXbar


  val plic = LazyModule(new TLPLIC(PLICParams(0x3c000000L), 8))
  val intSourceNode = IntSourceNode(IntSourcePortSimple(NrExtIntr, ports = 1, sources = 1))

  plic.intnode := intSourceNode
  plic.node := peripheralXbar

  val debugModule = LazyModule(new DebugModule(NumCores)(p))
  debugModule.debug.node := peripheralXbar
  debugModule.debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
    l3_xbar := TLBuffer() := TLWidthWidget(1) := sb2tl.node
  }
  lazy val module = new SoCMiscImp(this)
}
class SoCMiscImp(outer:SoCMisc)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val debug_module_io = IO(new DebugModuleIO(outer.NumCores))
  val ext_intrs = IO(Input(UInt(outer.NrExtIntr.W)))

  outer.debugModule.module.io <> debug_module_io

  // sync external interrupts
  require(outer.intSourceNode.out.head._1.length == ext_intrs.getWidth)
  for ((plic_in, interrupt) <- outer.intSourceNode.out.head._1.zip(ext_intrs.asBools)) {
    val ext_intr_sync = RegInit(0.U(3.W))
    ext_intr_sync := Cat(ext_intr_sync(1, 0), interrupt)
    plic_in := ext_intr_sync(2)
  }

  val freq = 100
  val cnt = RegInit((freq - 1).U)
  val tick = cnt === 0.U
  cnt := Mux(tick, (freq - 1).U, cnt - 1.U)
  outer.clint.module.io.rtcTick := tick
}
