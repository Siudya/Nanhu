package xiangshan

import chisel3._
import org.chipsalliance.cde.config.{Config, Parameters}
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}
import chisel3.util.{Valid, ValidIO}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors}
import freechips.rocketchip.tilelink._
import xs.utils.tl.TLLogger
import xs.utils.mbist.MbistInterface
import huancun.{HCCacheParamsKey, HuanCun}
import coupledL2.{CoupledL2, L2ParamKey}
import xs.utils.{DFTResetSignals, ResetGen}
import system.HasSoCParameter
import top.BusPerfMonitor
import utils.{IntBuffer, TLClientsMerger, TLEdgeBuffer}
import xs.utils.dft.{DftModBundle, EdtFuncBundle, EdtWrapper, HasIjtag, SIB, SOCC}
import xs.utils.perf.DebugOptionsKey
import xs.utils.sram.SramBroadcastBundle

class L1BusErrorUnitInfo(implicit val p: Parameters) extends Bundle with HasSoCParameter {
  val ecc_error = Valid(UInt(soc.PAddrBits.W))
}

class XSL1BusErrors()(implicit val p: Parameters) extends BusErrors {
  val icache = new L1BusErrorUnitInfo
  val dcache = new L1BusErrorUnitInfo
  val l2 = new L1BusErrorUnitInfo

  override def toErrorList: List[Option[(ValidIO[UInt], String, String)]] =
    List(
      Some(icache.ecc_error, "I_ECC", "Icache ecc error"),
      Some(dcache.ecc_error, "D_ECC", "Dcache ecc error"),
      Some(l2.ecc_error, "L2_ECC", "L2Cache ecc error")
    )
}

/**
  *   XSTileMisc contains every module except Core and L2 Cache
  */
class XSTileMisc()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  val l1_xbar = TLXbar()
  val mmio_xbar = TLXbar()
  val mmio_port = TLIdentityNode() // to L3
  val memory_port = TLIdentityNode()
  val beu = LazyModule(new BusErrorUnit(
    new XSL1BusErrors(), BusErrorUnitParams(0x38010000)
  ))
  val busPMU = BusPerfMonitor(enable = !debugOpts.FPGAPlatform)
  val l1d_logger = TLLogger(s"L2_L1D_${coreParams.HartId}", !debugOpts.FPGAPlatform && debugOpts.EnableChiselDB)
  val l2_binder = coreParams.L2CacheParamsOpt.map(_ => BankBinder(coreParams.L2NBanks, 64))

  val coreUncachePort = TLTempNode()

  busPMU := l1d_logger
  l1_xbar :=* busPMU

  l2_binder match {
    case Some(binder) =>
      memory_port := TLBuffer.chainNode(2) := TLClientsMerger() := TLXbar() :=* binder
    case None =>
      memory_port := l1_xbar
  }

  mmio_xbar := TLBuffer.chainNode(1) := coreUncachePort
  beu.node := TLBuffer.chainNode(1) := mmio_xbar
  mmio_port := TLBuffer.chainNode(2) := mmio_xbar

  lazy val module = new XSTileMiscImp(this)
}
class XSTileMiscImp(outer:XSTileMisc)(implicit p: Parameters) extends LazyModuleImp(outer){
  val beu_errors = IO(Input(chiselTypeOf(outer.beu.module.io.errors)))
  outer.beu.module.io.errors <> beu_errors
}

class XSTile(val parentName:String = "Unknown")(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter {
  val core = LazyModule(new XSCore())
  val misc = LazyModule(new XSTileMisc())
  val l2cache = coreParams.L2CacheParamsOpt.map(l2param =>
    LazyModule(new CoupledL2()(new Config((_, _, _) => {
      case L2ParamKey => l2param.copy(hartIds = Seq(p(XSCoreParamsKey).HartId))
      case DebugOptionsKey => p(DebugOptionsKey)
    })))
  )

  // public ports
  val memory_port = misc.memory_port
  val uncache = misc.mmio_port
  val clint_int_sink = IntIdentityNode()
  val plic_int_sink = IntIdentityNode()
  val debug_int_sink = IntIdentityNode()
  val beu_int_source = IntIdentityNode()
  val l2_int_source = IntIdentityNode()
  val core_reset_sink = BundleBridgeSink(Some(() => Reset()))

  val cltIntBuf = LazyModule(new IntBuffer)
  val plicIntBuf = LazyModule(new IntBuffer)
  val dbgIntBuf = LazyModule(new IntBuffer)
  val beuIntBuf = LazyModule(new IntBuffer)
  val l2IntBuf = LazyModule(new IntBuffer)
  core.clint_int_sink :*= cltIntBuf.node :*= clint_int_sink
  core.plic_int_sink :*= plicIntBuf.node :*= plic_int_sink
  core.debug_int_sink :*= dbgIntBuf.node :*= debug_int_sink
  beu_int_source :*= beuIntBuf.node :*= misc.beu.intNode
  l2cache.foreach(l2 => l2_int_source :*= l2IntBuf.node :*= l2.intNode)

  val l1d_to_l2_bufferOpt = coreParams.dcacheParametersOpt.map { _ =>
    val buffer = LazyModule(new TLBuffer)
    misc.l1d_logger := buffer.node := core.exuBlock.memoryBlock.dcache.clientNode
    buffer
  }

  def chainBuffer(depth: Int, n: String): (Seq[LazyModule], TLNode) = {
    val buffers = Seq.fill(depth){ LazyModule(new TLBuffer()) }
    buffers.zipWithIndex.foreach{ case (b, i) =>
      b.suggestName(s"${n}_${i}")
    }
    val node = buffers.map(_.node.asInstanceOf[TLNode]).reduce(_ :*=* _)
    (buffers, node)
  }

  val (l1i_to_l2_buffers, l1i_to_l2_buf_node) = chainBuffer(3, "l1i_to_l2_buffer")
  misc.busPMU :=
    TLLogger(s"L2_L1I_${coreParams.HartId}", !debugOpts.FPGAPlatform && debugOpts.EnableChiselDB) :=
    l1i_to_l2_buf_node :=
    core.frontend.icache.clientNode

  val ptw_to_l2_buffers = if (!coreParams.softPTW) {
    val (buffers, buf_node) = chainBuffer(2, "ptw_to_l2_buffer")
    misc.busPMU :=
      TLLogger(s"L2_PTW_${coreParams.HartId}", !debugOpts.FPGAPlatform && debugOpts.EnableChiselDB) :=
      buf_node :=
      core.ptw_to_l2_buffer.node
    buffers
  } else Seq()

  val l2InputBuffers = if(l2cache.isDefined) Some(Seq.fill(2)(LazyModule(new TLBuffer()))) else None
  l2cache match {
    case Some(l2) =>
      misc.l2_binder.get :*= l2.node :*= l2InputBuffers.get(1).node :*= l2InputBuffers.get(0).node :*= misc.l1_xbar
      l2.pf_recv_node.map {l2_node =>
          println("Connecting L1 prefetcher to L2!")
          core.exuBlock.memoryBlock.pf_sender_opt.map(sender => l2_node := sender)
      }
    case None =>
  }

  misc.coreUncachePort := core.uncacheBuffer.node

  lazy val module = new XSTileImp(this)
}

class XsTileDftBundle(ci:Int, co:Int) extends Bundle {
  val scan_en: Bool = Input(Bool())
  val occ_shift:Bool = Input(Bool())
  val edt: EdtFuncBundle = new EdtFuncBundle(ci, co)
  val ram: SramBroadcastBundle = new SramBroadcastBundle
  val rstCtl: DFTResetSignals = Input(new DFTResetSignals)
}

class XSTileImp(outer: XSTile)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasSoCParameter
  with HasIjtag {
  val mName = "XSTile"
  val io = IO(new Bundle {
    val hartId = Input(UInt(64.W))
    val reset_vector = Input(UInt(PAddrBits.W))
    val cpu_halt = Output(Bool())
    val xsTileResetGate = Input(Bool())
  })
  private val ciw = p(XSCoreParamsKey).edtCi
  private val cow = p(XSCoreParamsKey).edtCo
  val dft = IO(new XsTileDftBundle(ciw, cow))

  dontTouch(io.hartId)
  private val sib = Module(new SIB)
  private val socc = Module(new SOCC)
  private val edt = Module(new EdtWrapper(
    mName = "TileEdtWrapper",
    instName = "edt",
    ciw = coreParams.edtCi,
    cow = coreParams.edtCo,
    sw = coreParams.edtScan,
    chainRange = coreParams.edtRange
  ))
  socc.io.scan := DontCare
  socc.io.scan.en := dft.scan_en
  socc.io.clock := clock
  socc.io.clkCtrl := true.B
  socc.io.shiftOnlyMode := dft.occ_shift
  makeChain(Seq(ijtag, sib.ijtag))
  makeChain(Seq(sib.host, socc.ijtag, edt.io.ijtag, outer.core.module.ijtag) ++ outer.l2cache.map(_.module.ijtag))
  private val tileClock = socc.io.outClock
  outer.core.module.io.hartId := io.hartId
  outer.core.module.io.reset_vector := io.reset_vector
  outer.core.module.io.dfx_reset := dft.rstCtl
  edt.io.scan_en := dft.scan_en
  edt.io.ci := dft.edt.in_channels
  dft.edt.out_channels := edt.io.co
  edt.io.update := dft.edt.update
  edt.io.atpg_clock := clock

  private val l2bufs = outer.l2InputBuffers.getOrElse(Seq())
  private val bufSeq = (outer.l1i_to_l2_buffers ++ outer.ptw_to_l2_buffers ++ l2bufs).map(_.module.asInstanceOf[LazyModuleImp])
  private val modSeq = Seq(
    outer.misc.module,
    outer.core.module,
    outer.cltIntBuf.module,
    outer.plicIntBuf.module,
    outer.dbgIntBuf.module,
    outer.beuIntBuf.module,
    outer.l2IntBuf.module
  ) ++ bufSeq ++ outer.l2cache.map(_.module.asInstanceOf[LazyModuleImp]) ++
    outer.l1d_to_l2_bufferOpt.map(_.module.asInstanceOf[LazyModuleImp])
  modSeq.foreach(_.clock := tileClock)

  outer.l2cache.foreach(_.module.io.dfx_reset := dft.rstCtl)
  io.cpu_halt := outer.core.module.io.cpu_halt

  if (outer.l2cache.isDefined) {
    require(outer.core.module.io.perfEvents.length == outer.l2cache.get.module.io_perf.length)
    outer.core.module.io.perfEvents.zip(outer.l2cache.get.module.io_perf).foreach(x => x._1.value := x._2.value)
  }
  else {
    outer.core.module.io.perfEvents <> DontCare
  }

  outer.misc.module.beu_errors.icache <> outer.core.module.io.beu_errors.icache
  outer.misc.module.beu_errors.dcache <> outer.core.module.io.beu_errors.dcache
  outer.misc.module.beu_errors.l2 <> 0.U.asTypeOf(outer.misc.module.beu_errors.l2)

  if(outer.coreParams.hasMbist) {
    outer.core.module.dft.get := dft.ram
  }
  if(outer.coreParams.L2CacheParamsOpt.isDefined) {
    if (outer.coreParams.L2CacheParamsOpt.get.hasMbist) {
      outer.l2cache.get.module.dft.get := dft.ram
    }
  }
  // Modules are reset one by one
  // io_reset ----
  //             |
  //             v
  // reset ----> OR_SYNC --> {Misc, L2 Cache, Cores}

  val resetChain = Seq(
    Seq(
      outer.misc.module,
      outer.core.module,
      outer.cltIntBuf.module,
      outer.plicIntBuf.module,
      outer.dbgIntBuf.module,
      outer.beuIntBuf.module,
      outer.l2IntBuf.module
    ) ++ outer.l2cache.map(_.module),
    outer.l1i_to_l2_buffers.map(_.module.asInstanceOf[Module]) ++
      outer.ptw_to_l2_buffers.map(_.module.asInstanceOf[Module]) ++
      l2bufs.map(_.module.asInstanceOf[Module]) ++
      outer.l1d_to_l2_bufferOpt.map(_.module)
  )
  val gatedReset = Wire(Reset())
  gatedReset := (reset.asBool | io.xsTileResetGate).asAsyncReset
  ResetGen(resetChain, gatedReset, Some(dft.rstCtl), !outer.debugOpts.FPGAPlatform)
}
