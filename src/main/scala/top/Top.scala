/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * ************************************************************************************* */

package top

import chisel3._
import xiangshan._
import utils._
import system._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.FirtoolOption
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.jtag.JTAGIO
import xs.utils.{DFTResetSignals, FileRegisters, ResetGen}
import xs.utils.sram.BroadCastBundle
import huancun.{HCCacheParamsKey, HuanCun}
import xs.utils.mbist.{MBISTInterface, MBISTPipeline}
import xs.utils.perf.DebugOptionsKey

abstract class BaseXSSoc()(implicit p: Parameters) extends LazyModule
  with BindingScope {

  lazy val dts = DTS(bindingTree)
  lazy val json = JSON(bindingTree)
}

class XSTop()(implicit p: Parameters) extends BaseXSSoc() with HasSoCParameter {
  println(s"FPGASoC cores: $NumCores banks: $L3NBanks block size: $L3BlockSize bus size: $L3OuterBusWidth")

  val core_with_l2 = tiles.zipWithIndex.map({ case (coreParams, idx) =>
    LazyModule(new XSTile(s"XSTop_XSTile_")(p.alterPartial({
      case XSCoreParamsKey => coreParams
    })))
  })

  val misc = LazyModule(new SoCMisc())

  val l3cacheOpt = soc.L3CacheParamsOpt.map(l3param =>
    LazyModule(new HuanCun("L3_")(new Config((_, _, _) => {
      case HCCacheParamsKey => l3param.copy(enableTopDown = debugOpts.EnableTopDown)
      case DebugOptionsKey => p(DebugOptionsKey)
    })))
  )

  ResourceBinding {
    val width = ResourceInt(2)
    val model = "freechips,rocketchip-unknown"
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "compat").bind(ResourceString(model + "-dev"))
    Resource(ResourceAnchors.soc, "compat").bind(ResourceString(model + "-soc"))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc, "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))

    def bindManagers(xbar: TLNexusNode): Unit = {
      ManagerUnification(xbar.edges.in.head.manager.managers).foreach { manager =>
        manager.resources.foreach(r => r.bind(manager.toResource))
      }
    }

    bindManagers(misc.l3_xbar.asInstanceOf[TLNexusNode])
    bindManagers(misc.peripheralXbar.asInstanceOf[TLNexusNode])
  }

  l3cacheOpt.map(_.ctlnode.map(_ := misc.peripheralXbar))
  l3cacheOpt.map(_.intnode.map(int => {
    misc.periCx.plic.intnode := IntBuffer() := int
  }))

  for (i <- 0 until NumCores) {
    core_with_l2(i).clint_int_sink := misc.periCx.clint.intnode
    core_with_l2(i).plic_int_sink :*= misc.periCx.plic.intnode
    core_with_l2(i).debug_int_sink := misc.periCx.debugModule.debug.dmOuter.dmOuter.intnode
    misc.periCx.plic.intnode := IntBuffer() := core_with_l2(i).beu_int_source
    misc.periCx.plic.intnode := IntBuffer() := core_with_l2(i).l2_int_source
    misc.peripheral_ports(i) := core_with_l2(i).uncache
    misc.core_to_l3_ports(i) :=* core_with_l2(i).memory_port
  }

  (core_with_l2.head.l2cache.get.spp_send_node, core_with_l2.last.l2cache.get.spp_send_node) match {
    case (Some(l2_0), Some(l2_1)) => {
      val l3pf_RecvXbar = LazyModule(new coupledL2.prefetch.PrefetchReceiverXbar(NumCores))
      for (i <- 0 until NumCores) {
        println(s"Connecting L2 prefecher_sender_${i} to L3!")
        l3pf_RecvXbar.inNode(i) := core_with_l2(i).l2cache.get.spp_send_node.get
      }
      l3cacheOpt.get.pf_l3recv_node.map(l3Recv => l3Recv := l3pf_RecvXbar.outNode.head)
    }
    case (_, _) => None
  }

  // val core_rst_nodes = if(l3cacheOpt.nonEmpty && l3cacheOpt.get.rst_nodes.nonEmpty){
  //   l3cacheOpt.get.rst_nodes.get
  // } else {
  //   core_with_l2.map(_ => BundleBridgeSource(() => Reset()))
  // }
  val core_rst_nodes = core_with_l2.map(_ => BundleBridgeSource(() => Reset()))

  core_rst_nodes.zip(core_with_l2.map(_.core_reset_sink)).foreach({
    case (source, sink) => sink := source
  })

  l3cacheOpt match {
    case Some(l3) =>
      misc.l3_out :*= l3.node :*= TLBuffer.chainNode(2) :*= misc.l3_banked_xbar
    case None =>
  }

  lazy val module = new Impl

  class Impl extends LazyRawModuleImp(this) {
    FileRegisters.add("dts", dts)
    FileRegisters.add("graphml", graphML)
    FileRegisters.add("json", json)
    FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())

    val dma = IO(Flipped(misc.dma.cloneType))
    val peripheral = IO(misc.peripheral.cloneType)
    val memory = IO(misc.memory.cloneType)

    misc.dma <> dma
    peripheral <> misc.peripheral
    memory <> misc.memory

    val io = IO(new Bundle {
      val clock = Input(Bool())
      val reset = Input(AsyncReset())
      val extIntrs = Input(UInt(NrExtIntr.W))
      val systemjtag = new Bundle {
        val jtag = Flipped(new JTAGIO(hasTRSTn = false))
        val reset = Input(AsyncReset()) // No reset allowed on top
        val mfr_id = Input(UInt(11.W))
        val part_number = Input(UInt(16.W))
        val version = Input(UInt(4.W))
      }
      val debug_reset = Output(Bool())
      val riscv_halt = Output(Vec(NumCores, Bool()))
      val riscv_rst_vec = Input(Vec(NumCores, UInt(soc.PAddrBits.W)))
      val debugCores = Vec(NumCores, new CoreDbgIO()(p.alterPartial({
        case XSCoreParamsKey => tiles.head
      })))
      val debugL2s = Vec(NumCores, chiselTypeOf(core_with_l2.head.module.io.debugL2))
    })

    val scan_mode = IO(Input(Bool()))
    val dft_lgc_rst_n = IO(Input(AsyncReset()))
    val dft_mode = IO(Input(Bool()))
    val rtc_clock = IO(Input(Bool()))
    val dfx_reset = Wire(new DFTResetSignals())
    dfx_reset.lgc_rst_n := dft_lgc_rst_n
    dfx_reset.mode := dft_mode
    dfx_reset.scan_mode := scan_mode

    val reset_sync = withClockAndReset(io.clock.asClock, io.reset) {
      ResetGen(2, Some(dfx_reset))
    }
    val jtag_reset_sync = withClockAndReset(io.systemjtag.jtag.TCK, io.systemjtag.reset) {
      ResetGen(2, Some(dfx_reset))
    }

    // override LazyRawModuleImp's clock and reset
    childClock := io.clock.asClock
    childReset := reset_sync

    // output
    io.debug_reset := misc.module.debug_module_io.debugIO.ndreset

    // input
    dontTouch(dma)
    dontTouch(io)
    dontTouch(peripheral)
    dontTouch(memory)
    dontTouch(scan_mode)
    dontTouch(dft_lgc_rst_n)
    dontTouch(dft_mode)
    dontTouch(dfx_reset)
    misc.module.ext_intrs := io.extIntrs
    misc.module.dfx_reset := dfx_reset
    misc.module.rtc_clock := rtc_clock

    for ((core, i) <- core_with_l2.zipWithIndex) {
      core.module.io.hartId := i.U
      core.module.io.dfx_reset := dfx_reset
      core.module.io.reset_vector := io.riscv_rst_vec(i)
      io.riscv_halt(i) := core.module.io.cpu_halt
      io.debugCores(i) := core.module.io.debugCore
      io.debugL2s(i) := core.module.io.debugL2
    }
    core_rst_nodes.foreach(_.out.head._1 := false.B.asAsyncReset)

    if (l3cacheOpt.isDefined) {
      if (l3cacheOpt.get.module.dfx_reset.isDefined) {
        l3cacheOpt.get.module.dfx_reset.get := dfx_reset
      }
    }

    misc.module.debug_module_io.resetCtrl.hartIsInReset := core_with_l2.map(_.module.ireset.asBool)
    misc.module.debug_module_io.clock := io.clock
    misc.module.debug_module_io.reset := misc.module.reset

    misc.module.debug_module_io.debugIO.reset := misc.module.reset
    misc.module.debug_module_io.debugIO.clock := io.clock.asClock
    // TODO: delay 3 cycles?
    misc.module.debug_module_io.debugIO.dmactiveAck := misc.module.debug_module_io.debugIO.dmactive
    // jtag connector
    misc.module.debug_module_io.debugIO.systemjtag.foreach { x =>
      x.jtag <> io.systemjtag.jtag
      x.reset := jtag_reset_sync
      x.mfr_id := io.systemjtag.mfr_id
      x.part_number := io.systemjtag.part_number
      x.version := io.systemjtag.version
    }

    val mbistBroadCastToTile = if (core_with_l2.head.module.dft.isDefined) {
      val res = Some(Wire(new BroadCastBundle))
      core_with_l2.foreach(_.module.dft.get := res.get)
      res
    } else {
      None
    }
    val mbistBroadCastToL3 = if (l3cacheOpt.isDefined) {
      if (l3cacheOpt.get.module.dft.isDefined) {
        val res = Some(Wire(new BroadCastBundle))
        l3cacheOpt.get.module.dft.get := res.get
        res
      } else {
        None
      }
    } else {
      None
    }
    val mbistBroadCastToMisc = if (misc.module.dft.isDefined) {
      val res = Some(Wire(new BroadCastBundle))
      misc.module.dft.get := res.get
      res
    } else {
      None
    }

    class DftBundle extends Bundle {
      val ram_hold = Input(Bool())
      val ram_bypass = Input(Bool())
      val ram_bp_clken = Input(Bool())
      val l3dataram_clk = Input(Bool())
      val l3dataramclk_bypass = Input(Bool())
      val cgen = Input(Bool())
    }
    class SramBundle extends Bundle {
      val rf2p_ctrl = Input(UInt(20.W))
      val rmsp_hd_ctrl = Input(UInt(13.W))
      val rmsp_hs_ctrl = Input(UInt(17.W))
    }

    val dft = if (mbistBroadCastToTile.isDefined || mbistBroadCastToL3.isDefined || mbistBroadCastToMisc.isDefined) {
      Some(IO(new DftBundle))
    } else {
      None
    }
    val sram = if (mbistBroadCastToTile.isDefined || mbistBroadCastToL3.isDefined || mbistBroadCastToMisc.isDefined) {
      Some(IO(new SramBundle))
    } else {
      None
    }
    dft.foreach(dontTouch(_))
    sram.foreach(dontTouch(_))
    (mbistBroadCastToTile ++ mbistBroadCastToL3 ++ mbistBroadCastToMisc).foreach(b => {
      b.ram_hold            := dft.get.ram_hold
      b.ram_bypass          := dft.get.ram_bypass
      b.ram_bp_clken        := dft.get.ram_bp_clken
      b.l3dataram_clk       := dft.get.l3dataram_clk
      b.l3dataramclk_bypass := dft.get.l3dataramclk_bypass
      b.cgen                := dft.get.cgen
      b.rf2p_ctrl           := sram.get.rf2p_ctrl
      b.rmsp_hd_ctrl        := sram.get.rmsp_hd_ctrl
      b.rmsp_hs_ctrl        := sram.get.rmsp_hs_ctrl
    })

    /** ***************************************l3 & misc Mbist Share Bus************************************** */
    withClockAndReset(io.clock.asClock, reset_sync) {
      val miscPipeLine = if (p(SoCParamsKey).hasMbist && p(SoCParamsKey).hasShareBus) {
        MBISTPipeline.PlaceMbistPipeline(Int.MaxValue, s"MBIST_L3", true)
      } else {
        None
      }
      val miscIntf = if (p(SoCParamsKey).hasMbist && p(SoCParamsKey).hasShareBus) {
        Some(miscPipeLine.zipWithIndex.map({ case (pip, idx) => {
          val params = pip.nodeParams
          val intf = Module(new MBISTInterface(
            params = Seq(params),
            ids = Seq(pip.childrenIds),
            name = s"MBIST_intf_misc",
            pipelineNum = 1
          ))
          intf.toPipeline.head <> pip.mbist
          intf.mbist := DontCare
          pip.genCSV(intf.info, s"MBIST_MISC")
          dontTouch(intf.mbist)
          //TODO: add mbist controller connections here
          intf
        }
        }))
      } else {
        None
      }

      // Modules are reset one by one
      // reset ----> SYNC --> {SoCMisc, L3 Cache, Cores}
      val coreResetChain: Seq[Reset] = core_with_l2.map(_.module.ireset)
      val resetChain = Seq(misc.module.reset) ++ l3cacheOpt.map(_.module.reset) ++ coreResetChain
      val resetDftSigs = ResetGen.applyOneLevel(resetChain, reset_sync, !debugOpts.FPGAPlatform)
      resetDftSigs := dfx_reset
    }
  }
}

object TopMain extends App {
  val (config, firrtlOpts) = ArgParser.parse(args)
  xsphase.PrefixHelper.prefix = config(PrefixKey)
  val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)
  (new XiangShanStage).execute(firrtlOpts, Seq(
    FirtoolOption("-O=release"),
    FirtoolOption("--disable-all-randomization"),
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--strip-debug-info"),
    FirtoolOption("--lower-memories"),
    FirtoolOption("--add-vivado-ram-address-conflict-synthesis-bug-workaround"),
    FirtoolOption("--lowering-options=noAlwaysComb," +
      " disallowPortDeclSharing, disallowLocalVariables," +
      " emittedLineLength=120, explicitBitcast, locationInfoStyle=plain," +
      " disallowExpressionInliningInPorts, disallowMuxInlining"),
    ChiselGeneratorAnnotation(() => {
      soc.module
    })
  ))
  FileRegisters.write(filePrefix = config(PrefixKey) + "XSTop.")
}
