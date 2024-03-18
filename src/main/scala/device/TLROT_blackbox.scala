package device_rot

import chisel3._
import chisel3.RawModule
// import Chisel._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import chisel3.experimental._
import freechips.rocketchip.regmapper.{RegField, RegFieldAccessType, RegFieldDesc, RegFieldGroup}

class interruptIO extends Bundle{
    val intr_hmac_hmac_done_o = Output(Bool())
    val intr_hmac_fifo_empty_o = Output(Bool())  
    val intr_hmac_hmac_err_o = Output(Bool())

    val intr_kmac_kmac_done_o = Output(Bool())
    val intr_kmac_fifo_empty_o = Output(Bool())
    val intr_kmac_kmac_err_o = Output(Bool())

    val intr_keymgr_op_done_o = Output(Bool())

    val intr_csrng_cs_cmd_req_done_o = Output(Bool())
    val intr_csrng_cs_entropy_req_o = Output(Bool())
    val intr_csrng_cs_hw_inst_exc_o = Output(Bool())
    val intr_csrng_cs_fatal_err_o = Output(Bool())

    val intr_entropy_src_es_entropy_valid_o = Output(Bool())
    val intr_entropy_src_es_health_test_failed_o = Output(Bool())
    val intr_entropy_src_es_observe_fifo_ready_o = Output(Bool())
    val intr_entropy_src_es_fatal_err_o = Output(Bool())

    val intr_edn0_edn_cmd_req_done_o = Output(Bool())
    val intr_edn0_edn_fatal_err_o = Output(Bool())
}


class TLROT_top extends BlackBox with HasBlackBoxResource {
  val bundleParams  = TLBundleParameters(
  addressBits = 32,
  dataBits = 32,
  sourceBits = 8,
  sinkBits = 8,
  sizeBits = 3,
  echoFields = Seq(),
  requestFields = Seq(),
  responseFields = Seq(),
  hasBCE = false 
)
  val TL_AW = UInt(32.W)
  val TL_DW = UInt(32.W)
  val TL_AIW = UInt(8.W)
  val TL_DIW = UInt(1.W)
  val TL_DBW = UInt(4.W)
  val TL_SZW = UInt(2.W)

  val TL_DW64 = UInt(64.W)
  val TL_DBW64 = UInt(8.W)
  val TL_SZW64 = UInt(3.W)

  val io = IO(new Bundle {
    val clk_i = Input(Clock())
    // val rst_ni = Input(AsyncReset())
    val rst_ni = Input(Bool())
    val ROMInitEn = Output(Bool())
    // val tl_i = Input(new TlH2d())
    // val tl_o = Output(new TlD2h())

    // val tl_i = Input(new TLBundleA(bundleParams))
    // val tl_i = Input(Decoupled(new TLBundleA(bundleParams)))

    // val tl_o = Output(Decoupled(new TLBundleD(bundleParams)))

    // val done_o = Output(Bool())
    

    val a_valid = Input(Bool())
    val a_bits_opcode = Input(UInt(3.W))
    val a_bits_param = Input(UInt(3.W))
    val a_bits_size = Input(TL_SZW)
    val a_bits_source = Input(TL_AIW)
    val a_bits_address = Input(TL_AW)
    val a_bits_mask = Input(TL_DBW)
    val a_bits_data = Input(TL_DW)
    val a_ready = Output(Bool())

    val d_valid = Output(Bool())
    val d_bits_opcode = Output(UInt(3.W))
    val d_bits_param = Output(UInt(3.W))
    val d_bits_size = Output(TL_SZW)
    val d_bits_source = Output(TL_AIW)
    val d_bits_sink = Output(TL_DIW)
    val d_bits_data = Output(TL_DW)
    val d_bits_denied = Output(Bool())
    val d_ready = Input(Bool())

    val a_valid_rom = Input(Bool())
    val a_bits_opcode_rom = Input(UInt(3.W))
    val a_bits_param_rom = Input(UInt(3.W))
    val a_bits_size_rom = Input(TL_SZW64)
    val a_bits_source_rom = Input(TL_AIW)
    val a_bits_address_rom = Input(TL_AW)
    val a_bits_mask_rom = Input(TL_DBW64) 
    val a_bits_data_rom = Input(TL_DW64)
    val a_ready_rom = Output(Bool())

    val d_valid_rom = Output(Bool())
    val d_bits_opcode_rom = Output(UInt(3.W))
    val d_bits_param_rom = Output(UInt(3.W))
    val d_bits_size_rom = Output(TL_SZW64)
    val d_bits_source_rom = Output(TL_AIW)
    val d_bits_sink_rom = Output(TL_DIW)
    val d_bits_data_rom = Output(TL_DW64)
    val d_bits_denied_rom = Output(Bool())
    val d_ready_rom = Input(Bool())

    val key0 = Input(UInt(256.W))
    val key_valid = Input(Bool())

    val intr_hmac_hmac_done_o = Output(Bool())
    val intr_hmac_fifo_empty_o = Output(Bool())  
    val intr_hmac_hmac_err_o = Output(Bool())

    val intr_kmac_kmac_done_o = Output(Bool())
    val intr_kmac_fifo_empty_o = Output(Bool())
    val intr_kmac_kmac_err_o = Output(Bool())

    val intr_keymgr_op_done_o = Output(Bool())

    val intr_csrng_cs_cmd_req_done_o = Output(Bool())
    val intr_csrng_cs_entropy_req_o = Output(Bool())
    val intr_csrng_cs_hw_inst_exc_o = Output(Bool())
    val intr_csrng_cs_fatal_err_o = Output(Bool())

    val intr_entropy_src_es_entropy_valid_o = Output(Bool())
    val intr_entropy_src_es_health_test_failed_o = Output(Bool())
    val intr_entropy_src_es_observe_fifo_ready_o = Output(Bool())
    val intr_entropy_src_es_fatal_err_o = Output(Bool())

    val intr_edn0_edn_cmd_req_done_o = Output(Bool())
    val intr_edn0_edn_fatal_err_o = Output(Bool())
  })

  addResource("/TLROT/TLROT_top.sv")
  // addResource("/tlul_pkg.sv")
}



class TLROT_blackbox(implicit p: Parameters) extends LazyModule {
  // val device = new SimpleDevice("tlrot", Seq("sifive,dtim0"))
  val beatBytes = 4
  // val mem = SyncReadMem(0x1000, UInt(32.W))
  // val regmap = RegField.map("mem" -> mem)

  // val tlrotAddr = ResourceAddress(
  //   address = Seq(AddressSet(0x39000000L, 0xffffff)),
  //   permissions = ResourcePermissions(
  //     r = true,
  //     w = true,
  //     x = false, 
  //     c = false,
  //     a = false
  //   )
  // )
  val tlrotDevice = new SimpleDevice("tlrot", Seq("sifive,tlrot0"))
  val tlrotResource = Resource(tlrotDevice, "tlrot")

  val tlrotDevice_rom = new SimpleDevice("tlrot_rom", Seq("sifive,tlrot_rom"))
  val tlrotResource_rom = Resource(tlrotDevice_rom, "tlrot_rom")

  // val tlrot = Module(new TLROT_top)

  // Create a TLManagerNode
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address = Seq(AddressSet(0x3b100000, 0xfffff)), 
    // resources = device.reg("mem"),
    resources = Seq(
      // tlrotAddr, 
      tlrotResource
    ),
    regionType         = RegionType.IDEMPOTENT,
    supportsGet        = TransferSizes(1, beatBytes),
    supportsPutFull    = TransferSizes(1, beatBytes),
    supportsPutPartial = TransferSizes(1, beatBytes),
    fifoId             = Some(0))),
     beatBytes)))

   // Create a TLManagerNode
  val beatBytes_rom = 8
  val node_rom = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address = Seq(AddressSet(0x3b200000, 0x07ffff)), 
    // resources = device.reg("mem"),
    resources = Seq(
      // tlrotAddr, 
      tlrotResource_rom
    ),
    regionType         = RegionType.IDEMPOTENT,
    supportsGet        = TransferSizes(beatBytes_rom, beatBytes_rom),
    // supportsPutFull    = TransferSizes(1, beatBytes_rom),
    // supportsPutPartial = TransferSizes(1, beatBytes_rom),
    fifoId             = Some(0))),
     beatBytes_rom))) 

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
  // lazy val module = new LazyModuleImp(this) {
    val io_rot = IO(new Bundle { 
      val clock = Input(Clock())
      // val reset = Input(AsyncReset())
      val reset = Input(Bool())
      val intr = Output(Vec(17,Bool()))
      val ROMInitEn = Output(Bool())
      val key0 = Input(UInt(256.W))
      val key_valid = Input(Bool())
      // val intr = Output(new interruptIO)

      // val intr_hmac_hmac_done_o = Output(Bool()) 
      // val intr_hmac_fifo_empty_o = Output(Bool())  
      // val intr_hmac_hmac_err_o = Output(Bool())

      // val intr_kmac_kmac_done_o = Output(Bool())
      // val intr_kmac_fifo_empty_o = Output(Bool())
      // val intr_kmac_kmac_err_o = Output(Bool())

      // val intr_keymgr_op_done_o = Output(Bool())

      // val intr_csrng_cs_cmd_req_done_o = Output(Bool())
      // val intr_csrng_cs_entropy_req_o = Output(Bool())
      // val intr_csrng_cs_hw_inst_exc_o = Output(Bool())
      // val intr_csrng_cs_fatal_err_o = Output(Bool())

      // val intr_entropy_src_es_entropy_valid_o = Output(Bool())
      // val intr_entropy_src_es_health_test_failed_o = Output(Bool())
      // val intr_entropy_src_es_observe_fifo_ready_o = Output(Bool())
      // val intr_entropy_src_es_fatal_err_o = Output(Bool())

      // val intr_edn0_edn_cmd_req_done_o = Output(Bool())
      // val intr_edn0_edn_fatal_err_o = Output(Bool())
  })
    val (in, edge) = node.in(0)
    dontTouch(in)

    val (in_rom, edge_rom) = node_rom.in(0)
    dontTouch(in_rom)

    dontTouch(io_rot.reset)

    // val rot = withClock(clock) {
    //   // val tlrot = Module(new TLROT)
    //    Module(new TLROT)
    //  }    
    val tlrot = Module(new TLROT_top)
    in.a.ready := tlrot.io.a_ready
    // tlrot.io.a_ready := in.a.ready
    tlrot.io.a_valid := in.a.valid
    tlrot.io.a_bits_opcode := in.a.bits.opcode
    tlrot.io.a_bits_param := in.a.bits.param
    tlrot.io.a_bits_size := in.a.bits.size
    tlrot.io.a_bits_source := in.a.bits.source
    tlrot.io.a_bits_address := in.a.bits.address
    tlrot.io.a_bits_mask := in.a.bits.mask
    tlrot.io.a_bits_data := in.a.bits.data

    // dontTouch(in.d.ready)
    // in.d.ready :=  tlrot.io.d_ready
    // tlrot.io.d_valid :=  in.d.valid
    in.d.valid := tlrot.io.d_valid
    tlrot.io.d_ready := in.d.ready
    in.d.bits.opcode := tlrot.io.d_bits_opcode
    in.d.bits.param := tlrot.io.d_bits_param
    in.d.bits.size := tlrot.io.d_bits_size
    in.d.bits.source := tlrot.io.d_bits_source
    in.d.bits.sink := tlrot.io.d_bits_sink
    in.d.bits.data := tlrot.io.d_bits_data
    in.d.bits.denied := tlrot.io.d_bits_denied

    
    in_rom.a.ready := tlrot.io.a_ready_rom
    // tlrot.io.a_ready := in.a.ready
    tlrot.io.a_valid_rom := in_rom.a.valid
    tlrot.io.a_bits_opcode_rom := in_rom.a.bits.opcode
    tlrot.io.a_bits_param_rom := in_rom.a.bits.param
    tlrot.io.a_bits_size_rom := in_rom.a.bits.size
    tlrot.io.a_bits_source_rom := in_rom.a.bits.source
    tlrot.io.a_bits_address_rom := in_rom.a.bits.address
    tlrot.io.a_bits_mask_rom := in_rom.a.bits.mask
    tlrot.io.a_bits_data_rom := in_rom.a.bits.data
    
    // dontTouch(in.d.ready)
    // in.d.ready :=  tlrot.io.d_ready
    // tlrot.io.d_valid :=  in.d.valid
    in_rom.d.valid := tlrot.io.d_valid_rom
    tlrot.io.d_ready_rom := in_rom.d.ready
    in_rom.d.bits.opcode := tlrot.io.d_bits_opcode_rom
    in_rom.d.bits.param := tlrot.io.d_bits_param_rom
    in_rom.d.bits.size := tlrot.io.d_bits_size_rom
    in_rom.d.bits.source := tlrot.io.d_bits_source_rom
    in_rom.d.bits.sink := tlrot.io.d_bits_sink_rom
    in_rom.d.bits.data := tlrot.io.d_bits_data_rom
    in_rom.d.bits.denied := tlrot.io.d_bits_denied_rom
    

    io_rot.intr(0) := tlrot.io.intr_hmac_hmac_done_o
    io_rot.intr(1) := tlrot.io.intr_hmac_fifo_empty_o
    io_rot.intr(2) := tlrot.io.intr_hmac_hmac_err_o

    io_rot.intr(3) := tlrot.io.intr_kmac_kmac_done_o
    io_rot.intr(4) := tlrot.io.intr_kmac_fifo_empty_o
    io_rot.intr(5) := tlrot.io.intr_kmac_kmac_err_o

    io_rot.intr(6) := tlrot.io.intr_keymgr_op_done_o

    io_rot.intr(7) := tlrot.io.intr_csrng_cs_cmd_req_done_o
    io_rot.intr(8) := tlrot.io.intr_csrng_cs_entropy_req_o
    io_rot.intr(9) := tlrot.io.intr_csrng_cs_hw_inst_exc_o
    io_rot.intr(10) := tlrot.io.intr_csrng_cs_fatal_err_o

    io_rot.intr(11) := tlrot.io.intr_entropy_src_es_entropy_valid_o
    io_rot.intr(12) := tlrot.io.intr_entropy_src_es_health_test_failed_o
    io_rot.intr(13) := tlrot.io.intr_entropy_src_es_observe_fifo_ready_o
    io_rot.intr(14) := tlrot.io.intr_entropy_src_es_fatal_err_o

    io_rot.intr(15) := tlrot.io.intr_edn0_edn_cmd_req_done_o
    io_rot.intr(16) := tlrot.io.intr_edn0_edn_fatal_err_o


    // in.a <> tlrot.io.tl_i
    // in.d <> tlrot.io.tl_o
    // in.d.ready := tlrot.io.tl_o.ready 
    tlrot.io.clk_i := io_rot.clock
    io_rot.ROMInitEn := tlrot.io.ROMInitEn

    tlrot.io.key0 := io_rot.key0
    tlrot.io.key_valid := io_rot.key_valid

    // val rst_wire = Wire(Reset())
    // rst_wire := io_rot.reset
    // tlrot.io.rst_ni := rst_wire
    tlrot.io.rst_ni := io_rot.reset
  }
}

