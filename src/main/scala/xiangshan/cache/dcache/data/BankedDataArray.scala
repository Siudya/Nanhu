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

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xs.utils.mbist.MBISTPipeline
import utils.{XSDebug, XSPerfAccumulate}
import xiangshan.backend.rob.RobPtr
import xs.utils.sram.SRAMTemplate

class L1BankedDataReadReq(implicit p: Parameters) extends DCacheBundle
{
  val way_en = Bits(DCacheWays.W)
  val addr = Bits(PAddrBits.W)
}
class L1BankedDataReadLsuReq(implicit p: Parameters) extends L1BankedDataReadReq{
  val robIdx = new RobPtr
  val kill = Bool()
}

class L1BankedDataReadLineReq(implicit p: Parameters) extends L1BankedDataReadReq
{
  val rmask = Bits(DCacheBanks.W)
}

// Now, we can write a cache-block in a single cycle
class L1BankedDataWriteReq(implicit p: Parameters) extends L1BankedDataReadReq
{
  val wmask = Bits(DCacheBanks.W)
  val data = Vec(DCacheBanks, Bits(DCacheSRAMRowBits.W))
}

// cache-block write request without data
class L1BankedDataWriteReqCtrl(implicit p: Parameters) extends L1BankedDataReadReq

class L1BankedDataReadResult(implicit p: Parameters) extends DCacheBundle
{
  // you can choose which bank to read to save power
  val ecc = Bits(eccBits.W)
  val raw_data = Bits(DCacheSRAMRowBits.W)
  val error_delayed = Bool() // 1 cycle later than data resp

  def asECCData() = {
    Cat(ecc, raw_data)
  }
}

class DataSRAMBankWriteReq(implicit p: Parameters) extends DCacheBundle {
  val en = Bool()
  val addr = UInt()
  val way_en = UInt(DCacheWays.W)
  val data = UInt(DCacheSRAMRowBits.W)
}

//                     Banked DCache Data
// -----------------------------------------------------------------
// | Bank0 | Bank1 | Bank2 | Bank3 | Bank4 | Bank5 | Bank6 | Bank7 |
// -----------------------------------------------------------------
// | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  |
// | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  |
// | ....  | ....  | ....  | ....  | ....  | ....  | ....  | ....  |
// -----------------------------------------------------------------
abstract class AbstractBankedDataArray(implicit p: Parameters) extends DCacheModule
{
  val ReadlinePortErrorIndex = LoadPipelineWidth
  val io = IO(new DCacheBundle {
    // load pipeline read word req
    val read = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1BankedDataReadLsuReq)))
    val readSel = Input(UInt(1.W))
    // main pipeline read / write line req
    val readline_intend = Input(Bool())
    val readline = Flipped(DecoupledIO(new L1BankedDataReadLineReq))
    val write = Flipped(DecoupledIO(new L1BankedDataWriteReq))
    val write_dup = Vec(DCacheBanks, Flipped(Decoupled(new L1BankedDataWriteReqCtrl)))
    // data bank read resp (all banks)
//    val resp = Output(Vec(DCacheBanks, new L1BankedDataReadResult()))
    val resp = Output(Vec(LoadPipelineWidth, new L1BankedDataReadResult()))
    val readline_resp = Output(Vec(DCacheBanks, new L1BankedDataReadResult()))
    // val nacks = Output(Vec(LoadPipelineWidth, Bool()))
    // val errors = Output(Vec(LoadPipelineWidth + 1, new L1CacheErrorInfo)) // read ports + readline port
    val read_error_delayed = Output(Vec(LoadPipelineWidth, Bool()))
    val readline_error_delayed = Output(Bool())
    // when bank_conflict, read (1) port should be ignored
    val bank_conflict_slow = Output(Vec(LoadPipelineWidth, Bool()))
    val bank_conflict_fast = Output(Vec(LoadPipelineWidth, Bool()))
    val disable_ld_fast_wakeup = Output(Vec(LoadPipelineWidth, Bool()))
    // customized cache op port 
    val cacheOp = Flipped(new L1CacheInnerOpIO)
    val cacheOp_req_dup = Vec(11, Flipped(Valid(new CacheCtrlReqInfo)))
    val cacheOp_req_bits_opCode_dup = Input(Vec(11, UInt(XLEN.W)))
  })
  assert(LoadPipelineWidth <= 2) // BankedDataArray is designed for no more than 2 read ports

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until LoadPipelineWidth).map(f))

  def dumpRead() = {
    (0 until LoadPipelineWidth) map { w =>
      when(io.read(w).valid) {
        XSDebug(s"DataArray Read channel: $w valid way_en: %x addr: %x\n",
          io.read(w).bits.way_en, io.read(w).bits.addr)
      }
    }
    when(io.readline.valid) {
      XSDebug(s"DataArray Read Line, valid way_en: %x addr: %x rmask %x\n",
        io.readline.bits.way_en, io.readline.bits.addr, io.readline.bits.rmask)
    }
  }

  def dumpWrite() = {
    when(io.write.valid) {
      XSDebug(s"DataArray Write valid way_en: %x addr: %x\n",
        io.write.bits.way_en, io.write.bits.addr)

      (0 until DCacheBanks) map { r =>
        XSDebug(s"cycle: $r data: %x wmask: %x\n",
          io.write.bits.data(r), io.write.bits.wmask(r))
      }
    }
  }

  def dumpResp() = {
    XSDebug(s"DataArray ReadeResp channel:\n")
    (0 until LoadPipelineWidth) map { r =>
      XSDebug(s"cycle: $r data: %x\n", io.resp(r).raw_data)
    }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpResp
  }
}

class BankedDataArray(parentName: String = "Unknown")(implicit p: Parameters) extends AbstractBankedDataArray {
  def getECCFromEncWord(encWord: UInt) = {
    require(encWord.getWidth == encWordBits)
    encWord(encWordBits - 1, wordBits)
  }

  val ReduceReadlineConflict = false

  io.write.ready := true.B
  io.write_dup.foreach(_.ready := true.B)

  // wrap data rows of 8 ways
  class DataSRAMBank(index: Int, parentName: String = "Unknown") extends Module {
    val io = IO(new Bundle() {
      val w = Input(new DataSRAMBankWriteReq)

      val r = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val way_en = Input(UInt(DCacheWays.W))
        val data = Output(UInt(DCacheSRAMRowBits.W))
      }
    })

    val r_way_en_reg = RegNext(io.r.way_en)

//    val w_reg = RegNext(io.w)
    val w_reg_en = RegNext(io.w.en)
    val w_reg_addr = RegEnable(io.w.addr,io.w.en)
    val w_reg_way_en = RegEnable(io.w.way_en,io.w.en)
    val w_reg_data = RegEnable(io.w.data,io.w.en)
    // val rw_bypass = RegNext(io.w.addr === io.r.addr && io.w.way_en === io.r.way_en && io.w.en)

    // multiway data bank
    val data_bank = Array.tabulate(DCacheWays) { idx =>
      Module(new SRAMTemplate(
        Bits(DCacheSRAMRowBits.W),
        set = DCacheSets,
        way = 1,
        shouldReset = false,
        holdRead = false,
        singlePort = true,
        hasMbist = coreParams.hasMbist,
        hasShareBus = coreParams.hasShareBus,
        parentName = parentName + s"bank${idx}_"
      ))
    }
    val mbistPipeline = if (coreParams.hasMbist && coreParams.hasShareBus) {
      Some(Module(new MBISTPipeline(1, s"${parentName}_mbistPipe")))
    } else {
      None
    }

    for (w <- 0 until DCacheWays) {
      val wen = w_reg_en && w_reg_way_en(w)
      data_bank(w).io.w.req.valid := wen
      data_bank(w).io.w.req.bits.apply(
        setIdx = w_reg_addr,
        data = w_reg_data,
        waymask = 1.U
      )
      data_bank(w).io.r.req.valid := io.r.en
      data_bank(w).io.r.req.bits.apply(setIdx = io.r.addr)
    }

    val half = nWays / 2
    val data_read = data_bank.map(_.io.r.resp.data(0))
    val data_left = Mux1H(r_way_en_reg.tail(half), data_read.take(half))
    val data_right = Mux1H(r_way_en_reg.head(half), data_read.drop(half))

    val sel_low = r_way_en_reg.tail(half).orR()
    val row_data = Mux(sel_low, data_left, data_right)

    io.r.data := row_data

    def dump_r() = {
      when(RegNext(io.r.en)) {
        XSDebug("bank read addr %x way_en %x data %x\n",
          RegNext(io.r.addr),
          RegNext(io.r.way_en),
          io.r.data
        )
      }
    }

    def dump_w() = {
      when(io.w.en) {
        XSDebug("bank write addr %x way_en %x data %x\n",
          io.w.addr,
          io.w.way_en,
          io.w.data
        )
      }
    }

    def dump() = {
      dump_w()
      dump_r()
    }
  }

  // wrap a sram
  class DataSRAM(bankIdx: Int, wayIdx: Int, parentName: String = "Unknown") extends Module {
    val io = IO(new Bundle() {
      val w = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val data = Input(UInt(DCacheSRAMRowBits.W))
      }

      val r = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val data = Output(UInt(DCacheSRAMRowBits.W))
      }
    })

    // data sram
    val data_sram = Module(new SRAMTemplate(
      Bits(DCacheSRAMRowBits.W),
      set = DCacheSets,
      way = 1,
      shouldReset = false,
      holdRead = false,
      singlePort = true,
      hasMbist = coreParams.hasMbist,
      hasShareBus = coreParams.hasShareBus,
      parentName = parentName + s"bank_${bankIdx}_way_${wayIdx}_"
    ))

    val wenReg = RegNext(io.w.en)
    val waddrReg = RegEnable(io.w.addr,io.w.en)
    val wdataReg = RegEnable(io.w.data,io.w.en)

    data_sram.io.w.req.valid := wenReg
    data_sram.io.w.req.bits.apply(
      setIdx = waddrReg,
      data = wdataReg,
      waymask = 1.U
    )
    data_sram.io.r.req.valid := io.r.en
    data_sram.io.r.req.bits.apply(setIdx = io.r.addr)
    io.r.data := data_sram.io.r.resp.data(0)

    def dump_r() = {
      when(RegNext(io.r.en)) {
        XSDebug("bank read set %x bank %x way %x data %x\n",
          RegNext(io.r.addr),
          bankIdx.U,
          wayIdx.U,
          io.r.data
        )
      }
    }

    def dump_w() = {
      when(io.w.en) {
        XSDebug("bank write set %x bank %x way %x data %x\n",
          io.w.addr,
          bankIdx.U,
          wayIdx.U,
          io.w.data
        )
      }
    }

    def dump() = {
      dump_w()
      dump_r()
    }
  }

  val data_banks = List.tabulate(DCacheBanks)(i => List.tabulate(DCacheWays)(j => Module(new DataSRAM(i, j, parentName = parentName + s"arrsy${i}"))))
  //  val data_banks = List.tabulate(DCacheBanks)(i => Module(new DataSRAMBank(i, parentName = parentName + s"array${i}_")))
  val mbistPipeline = if (coreParams.hasMbist && coreParams.hasShareBus) {
    Some(Module(new MBISTPipeline(2, s"${parentName}_mbistPipe")))
  } else {
    None
  }
  // val ecc_banks = List.fill(DCacheBanks)(Module(new SRAMTemplate(
  //   Bits(eccBits.W),
  //   set = DCacheSets,
  //   way = DCacheWays,
  //   shouldReset = false,
  //   holdRead = false,
  //   singlePort = true
  // )))

  //  data_banks.map(_.dump())
  data_banks.map(_.map(_.dump()))

  require(LoadPipelineWidth == 2)
  private val readValids = Cat(io.read.map(_.valid).reverse)
  //readSel is a selector between read #0 and #1, used when bank conflict happen.
  //True: select #1 False:select #0
  private val readSel = Mux(readValids === 1.U, false.B,
    Mux(readValids === 2.U, true.B,
      Mux(readValids === 3.U, io.read(0).bits.robIdx > io.read(1).bits.robIdx,
        false.B)))
  val way_en = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val way_en_reg = RegNext(way_en)
  val set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val bank_addrs = Wire(Vec(LoadPipelineWidth, UInt()))

  // read data_banks and ecc_banks
  // for single port SRAM, do not allow read and write in the same cycle
  val rwhazard = RegNext(io.write.valid)
  val rrhazard = false.B // io.readline.valid
  (0 until LoadPipelineWidth).foreach(rport_index => {
    set_addrs(rport_index) := addr_to_dcache_set(io.read(rport_index).bits.addr)
    bank_addrs(rport_index) := addr_to_dcache_bank(io.read(rport_index).bits.addr)

    io.read(rport_index).ready := !(rwhazard || rrhazard)

    // use way_en to select a way after data read out
    when(io.read(rport_index).fire && !io.read(rport_index).bits.kill) {
      assert(PopCount(io.read(rport_index).bits.way_en) <= 1.U)
    }
    way_en(rport_index) := io.read(rport_index).bits.way_en
  })
  io.readline.ready := !(rwhazard)

  // read conflict
//  val rr_bank_conflict = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y =>
//    bank_addrs(x) === bank_addrs(y) && io.read(x).valid && io.read(y).valid && io.read(x).bits.way_en === io.read(y).bits.way_en && set_addrs(x) =/= set_addrs(y)
//  ))
  val rr_bank_conflict = bank_addrs(0) === bank_addrs(1) && io.read(0).valid && io.read(1).valid && io.read(0).bits.way_en === io.read(1).bits.way_en && set_addrs(0) =/= set_addrs(1)

  val rrl_bank_conflict = Wire(Vec(LoadPipelineWidth, Bool()))
  if (ReduceReadlineConflict) {
    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict(i) := io.read(i).valid && io.readline.valid && io.readline.bits.rmask(bank_addrs(i)))
  } else {
    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict(i) := io.read(i).valid && io.readline.valid && io.readline.bits.way_en === way_en(i) && addr_to_dcache_set(io.readline.bits.addr) =/= set_addrs(i))
  }
  val rrl_bank_conflict_intend = Wire(Vec(LoadPipelineWidth, Bool()))
  if (ReduceReadlineConflict) {
    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict_intend(i) := io.read(i).valid && io.readline_intend && io.readline.bits.rmask(bank_addrs(i)))
  } else {
    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict_intend(i) := io.read(i).valid && io.readline_intend && io.readline.bits.way_en === way_en(i) && addr_to_dcache_set(io.readline.bits.addr) =/= set_addrs(i))
  }
  //
  //  // read each bank, get bank result
  //  val bank_result = Wire(Vec(DCacheBanks, new L1BankedDataReadResult()))
  //  dontTouch(bank_result)
  //  // val read_bank_error_delayed = Wire(Vec(DCacheBanks, Bool()))
  //  // dontTouch(read_bank_error_delayed)
  //  val rr_bank_conflict = bank_addrs(0) === bank_addrs(1) && io.read(0).valid && io.read(1).valid
  //  val rrl_bank_conflict = Wire(Vec(LoadPipelineWidth, Bool()))
  //  if (ReduceReadlineConflict) {
  //    rrl_bank_conflict(0) := io.read(0).valid && io.readline.valid && io.readline.bits.rmask(bank_addrs(0))
  //    rrl_bank_conflict(1) := io.read(1).valid && io.readline.valid && io.readline.bits.rmask(bank_addrs(1))
  //  } else {
  //    rrl_bank_conflict(0) := io.read(0).valid && io.readline.valid
  //    rrl_bank_conflict(1) := io.read(1).valid && io.readline.valid
  //  }
  //  val rrl_bank_conflict_intend = Wire(Vec(LoadPipelineWidth, Bool()))
  //  if (ReduceReadlineConflict) {
  //    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict_intend(i) := io.read(i).valid && io.readline_intend && io.readline.bits.rmask(bank_addrs(i)))
  //  } else {
  //    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict_intend(i) := io.read(i).valid && io.readline_intend)
  //  }

  //  val rw_bank_conflict = VecInit(Seq.tabulate(LoadPipelineWidth)(io.read(_).valid && rwhazard))
  //  val perf_multi_read = PopCount(io.read.map(_.valid)) >= 2.U
  //  (0 until LoadPipelineWidth).foreach(i => {
  //    val highPriority = if(i == 0) !readSel else readSel
  //    io.bank_conflict_fast(i) := rw_bank_conflict(i) || rrl_bank_conflict(i) ||
  //      (!highPriority && rr_bank_conflict)
  //    io.bank_conflict_slow(i) := RegNext(io.bank_conflict_fast(i))
  //    io.disable_ld_fast_wakeup(i) := rw_bank_conflict(i) || rrl_bank_conflict_intend(i) ||
  //      (!highPriority && rr_bank_conflict)
  //  })
  //  XSPerfAccumulate("data_array_multi_read", perf_multi_read)
  //  XSPerfAccumulate("data_array_rr_bank_conflict", rr_bank_conflict)
  //  XSPerfAccumulate("data_array_rrl_bank_conflict(0)", rrl_bank_conflict(0))
  //  XSPerfAccumulate("data_array_rrl_bank_conflict(1)", rrl_bank_conflict(1))
  //  XSPerfAccumulate("data_array_rw_bank_conflict_0", rw_bank_conflict(0))
  //  XSPerfAccumulate("data_array_rw_bank_conflict_1", rw_bank_conflict(1))
  //  XSPerfAccumulate("data_array_access_total", io.read(0).valid +& io.read(1).valid)
  //  XSPerfAccumulate("data_array_read_0", io.read(0).valid)
  //  XSPerfAccumulate("data_array_read_1", io.read(1).valid)
  //  XSPerfAccumulate("data_array_read_line", io.readline.valid)
  //  XSPerfAccumulate("data_array_write", io.write.valid)

  val rw_bank_conflict = VecInit(Seq.tabulate(LoadPipelineWidth)(io.read(_).valid && rwhazard))
  val perf_multi_read = PopCount(io.read.map(_.valid)) >= 2.U
  (0 until LoadPipelineWidth).foreach(i => {
//    io.bank_conflict_fast(i) := rw_bank_conflict(i) || rrl_bank_conflict(i) ||
//      (if (i == 0) 0.B else (0 until i).map(rr_bank_conflict(_)(i)).reduce(_ || _))
    io.bank_conflict_fast(i) := rw_bank_conflict(i) || rrl_bank_conflict(i) ||
      (rr_bank_conflict && (readSel =/= i.U))
    io.bank_conflict_slow(i) := RegNext(io.bank_conflict_fast(i))
    io.disable_ld_fast_wakeup(i) := rw_bank_conflict(i) || rrl_bank_conflict_intend(i) ||
      (rr_bank_conflict && (readSel =/= i.U))
//      (if (i == 0) 0.B else (0 until i).map(rr_bank_conflict(_)(i)).reduce(_ || _))
  })
  XSPerfAccumulate("data_array_multi_read", perf_multi_read)
  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_rr_bank_conflict", rr_bank_conflict)
  ))
  (0 until LoadPipelineWidth).foreach(i => {
    XSPerfAccumulate(s"data_array_rrl_bank_conflict_${i}", rrl_bank_conflict(i))
    XSPerfAccumulate(s"data_array_rw_bank_conflict_${i}", rw_bank_conflict(i))
    XSPerfAccumulate(s"data_array_read_${i}", io.read(i).valid)
  })
  XSPerfAccumulate("data_array_access_total", PopCount(io.read.map(_.valid)))
  XSPerfAccumulate("data_array_read_line", io.readline.valid)
  XSPerfAccumulate("data_array_write", io.write.valid)


  //  for (bank_index <- 0 until DCacheBanks) {
  //    //     Set Addr & Read Way Mask
  //    //
  //    //      Pipe 0      Pipe 1
  //    //        +           +
  //    //        |           |
  //    // +------+-----------+-------+
  //    //  X                        X
  //    //   X                      +------+ Bank Addr Match
  //    //    +---------+----------+
  //    //              |
  //    //     +--------+--------+
  //    //     |    Data Bank    |
  //    //     +-----------------+
  //    val bank_addr_matchs = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
  //      bank_addrs(i) === bank_index.U && io.read(i).valid
  //    })))
  //    val readline_match = Wire(Bool())
  //    if (ReduceReadlineConflict) {
  //      readline_match := io.readline.valid && io.readline.bits.rmask(bank_index)
  //    } else {
  //      readline_match := io.readline.valid
  //    }
  //    val bank_way_en = Mux(readline_match,
  //      io.readline.bits.way_en,
  //      Mux(readSel,
  //        Mux(bank_addr_matchs(1), way_en(1), way_en(0)),
  //        Mux(bank_addr_matchs(0), way_en(0), way_en(1))
  //      )
  //    )
  //    val bank_set_addr = Mux(readline_match,
  //      addr_to_dcache_set(io.readline.bits.addr),
  //      Mux(readSel,
  //        Mux(bank_addr_matchs(1), set_addrs(1), set_addrs(0)),
  //        Mux(bank_addr_matchs(0), set_addrs(0), set_addrs(1))
  //      )
  //    )
  //
  //    val read_enable = bank_addr_matchs.asUInt.orR || readline_match
  //
  //    // read raw data
  //    val data_bank = data_banks(bank_index)
  //    data_bank.io.r.en := read_enable
  //    data_bank.io.r.way_en := bank_way_en
  //    data_bank.io.r.addr := bank_set_addr
  //    bank_result(bank_index).raw_data := data_bank.io.r.data
  //
  //    // read ECC
  //    // val ecc_bank = ecc_banks(bank_index)
  //    // ecc_bank.io.r.req.valid := read_enable
  //    // ecc_bank.io.r.req.bits.apply(setIdx = bank_set_addr)
  //    bank_result(bank_index).ecc := 0.U.asTypeOf(bank_result(bank_index).ecc.cloneType)//Mux1H(RegNext(bank_way_en), ecc_bank.io.r.resp.data)
  //
  //    // use ECC to check error
  //    // val ecc_data = bank_result(bank_index).asECCData()
  //    // val ecc_data_delayed = RegEnable(ecc_data, RegNext(read_enable))
  //    bank_result(bank_index).error_delayed := 0.U.asTypeOf(bank_result(bank_index).error_delayed.cloneType)//dcacheParameters.dataCode.decode(ecc_data_delayed).error
  //    // read_bank_error_delayed(bank_index) := bank_result(bank_index).error_delayed
  //  }


  val read_result = Wire(Vec(DCacheBanks, Vec(DCacheWays, new L1BankedDataReadResult())))
  val read_error_delayed_result = Wire(Vec(DCacheBanks, Vec(DCacheWays, Bool())))
  dontTouch(read_result)
  dontTouch(read_error_delayed_result)

  for (bank_index <- 0 until DCacheBanks) {
    for (way_index <- 0 until DCacheWays) {
      val loadpipe_en = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
        bank_addrs(i) === bank_index.U && io.read(i).valid && way_en(i)(way_index)
      })))

      val loadReadEn = Wire(Vec(2,Bool()))
      loadReadEn.zipWithIndex.foreach({case(d,i) => d := loadpipe_en(i) && Mux(rr_bank_conflict,readSel === i.U,true.B)})


      val readline_en = Wire(Bool())
      if (ReduceReadlineConflict) {
        readline_en := io.readline.valid && io.readline.bits.rmask(bank_index) && io.readline.bits.way_en(way_index)
      } else {
        readline_en := io.readline.valid && io.readline.bits.way_en(way_index)
      }
      val sram_set_addr = Mux(readline_en,
        addr_to_dcache_set(io.readline.bits.addr),
        PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => loadReadEn(i) -> set_addrs(i)))
      )
      val read_en = loadReadEn.asUInt.orR || readline_en
      // read raw data
      val data_bank = data_banks(bank_index)(way_index)
      data_bank.io.r.en := read_en
      data_bank.io.r.addr := sram_set_addr
      //      val ecc_bank = ecc_banks(bank_index)(way_index)
      //      ecc_bank.io.r.req.valid := read_en
      //      ecc_bank.io.r.req.bits.apply(setIdx = sram_set_addr)

      read_result(bank_index)(way_index).raw_data := data_bank.io.r.data
      read_result(bank_index)(way_index).ecc := DontCare

      // use ECC to check error
      val ecc_data = read_result(bank_index)(way_index).asECCData()
      val ecc_data_delayed = RegEnable(ecc_data, RegNext(read_en))
      read_result(bank_index)(way_index).error_delayed := dcacheParameters.dataCode.decode(ecc_data_delayed).error
      read_error_delayed_result(bank_index)(way_index) := read_result(bank_index)(way_index).error_delayed
    }
  }


  // read result: expose banked read result 
  //  io.resp := bank_result
  (0 until LoadPipelineWidth).map(i => {
    io.resp(i) := read_result(RegNext(bank_addrs(i)))(RegNext(OHToUInt(way_en(i))))
  })

  (0 until DCacheBanks).map(i => {
    io.readline_resp(i) := read_result(i)(RegNext(OHToUInt(io.readline.bits.way_en)))
  })



  // error detection
  // normal read ports
  (0 until LoadPipelineWidth).map(rport_index => {
    // io.read_error_delayed(rport_index) := RegNext(RegNext(io.read(rport_index).fire())) && 
    //   read_bank_error_delayed(RegNext(RegNext(bank_addrs(rport_index)))) &&
    //   !RegNext(io.bank_conflict_slow(rport_index))
    io.read_error_delayed(rport_index) := 0.U.asTypeOf(io.read_error_delayed(rport_index).cloneType)

  })
  // readline port
  io.readline_error_delayed := 0.U.asTypeOf(io.readline_error_delayed)


  // write data_banks & ecc_banks
  val sram_waddr = addr_to_dcache_set(io.write.bits.addr)
  val sram_waddr_dup = io.write_dup.map(x => addr_to_dcache_set(x.bits.addr))

  for (bank_index <- 0 until DCacheBanks) {
    for (way_index <- 0 until DCacheWays) {
      // data write
      val data_bank = data_banks(bank_index)(way_index)
      data_bank.io.w.en := io.write_dup(bank_index).valid && io.write.bits.wmask(bank_index) && io.write_dup(bank_index).bits.way_en(way_index)
      data_bank.io.w.addr := sram_waddr_dup(bank_index)
      data_bank.io.w.data := io.write.bits.data(bank_index)
      // ecc write
      //      val ecc_bank = ecc_banks(bank_index)(way_index)
      //      ecc_bank.io.w.req.valid := RegNext(io.write_dup(bank_index).valid && io.write.bits.wmask(bank_index) && io.write_dup(bank_index).bits.way_en(way_index))
      //      ecc_bank.io.w.req.bits.apply(
      //        setIdx = RegNext(sram_waddr_dup(bank_index)),
      //        data = RegNext(getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index))))),
      //        waymask = 1.U
      //      )
      //      when(ecc_bank.io.w.req.valid) {
      //        XSDebug("write in ecc sram: bank %x set %x data %x waymask %x\n",
      //          bank_index.U,
      //          sram_waddr,
      //          getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index)))),
      //          io.write.bits.way_en
      //        );
      //      }
    }
  }



  //  for (bank_index <- 0 until DCacheBanks) {
  //    // data write
  //    val data_bank = data_banks(bank_index)
  //    data_bank.io.w.en := io.write_dup(bank_index).valid && io.write.bits.wmask(bank_index)
  //    data_bank.io.w.way_en := io.write_dup(bank_index).bits.way_en
  //    data_bank.io.w.addr := sram_waddr_dup(bank_index)
  //    data_bank.io.w.data := io.write.bits.data(bank_index)
  //
  //    // // ecc write
  //    // val ecc_bank = ecc_banks(bank_index)
  //    // ecc_bank.io.w.req.valid := RegNext(io.write_dup(bank_index).valid && io.write.bits.wmask(bank_index))
  //    // ecc_bank.io.w.req.bits.apply(
  //    //   setIdx = RegNext(sram_waddr_dup(bank_index)),
  //    //   data = RegNext(getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index))))),
  //    //   waymask = RegNext(io.write_dup(bank_index).bits.way_en)
  //    // )
  //    // when(ecc_bank.io.w.req.valid) {
  //    //   XSDebug("write in ecc sram: bank %x set %x data %x waymask %x\n",
  //    //     bank_index.U,
  //    //     sram_waddr,
  //    //     getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index)))),
  //    //     io.write.bits.way_en
  //    //   );
  //    // }
  //  }
  io.cacheOp := DontCare
  io.cacheOp_req_dup := DontCare
  io.cacheOp_req_bits_opCode_dup := DontCare
  // deal with customized cache op
  require(nWays <= 32)
  io.cacheOp.resp.bits := DontCare
  val cacheOpShouldResp = WireInit(false.B)
  // val eccReadResult = Wire(Vec(DCacheBanks, UInt(eccBits.W)))

  when(io.cacheOp.req.valid && CacheInstrucion.isReadData(io.cacheOp.req.bits.opCode)) {
    for (bank_index <- 0 until (DCacheBanks / 3)) {
      for (way_index <- 0 until DCacheWays) {
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.r.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.r.addr := io.cacheOp.req.bits.index
      }
    }
    cacheOpShouldResp := true.B
  }


  // when (io.cacheOp_req_dup(0).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(0))) {
  //   for (bank_index <- 0 until (DCacheBanks / 3)) {
  //     val ecc_bank = ecc_banks(bank_index)
  //     ecc_bank.io.r.req.valid := true.B
  //     ecc_bank.io.r.req.bits.setIdx := io.cacheOp.req.bits.index
  //   }
  //   cacheOpShouldResp := true.B
  // }


  when(io.cacheOp_req_dup(1).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(1))) {
    for (bank_index <- 0 until (DCacheBanks / 3)) {
      for (way_index <- 0 until DCacheWays) {
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.w.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.w.addr := io.cacheOp.req.bits.index
        data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bank_index)
      }
    }
    cacheOpShouldResp := true.B
  }
  // when(io.cacheOp_req_dup(2).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(2))){
  //   for (bank_index <- 0 until (DCacheBanks / 3)) {
  //     val ecc_bank = ecc_banks(bank_index)
  //     ecc_bank.io.w.req.valid := true.B
  //     ecc_bank.io.w.req.bits.apply(
  //       setIdx = io.cacheOp.req.bits.index,
  //       data = io.cacheOp.req.bits.write_data_ecc,
  //       waymask = UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
  //     )
  //   }
  //   cacheOpShouldResp := true.B
  // }


  when(io.cacheOp_req_dup(3).valid && CacheInstrucion.isReadData(io.cacheOp_req_bits_opCode_dup(3))) {
    for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
      for (way_index <- 0 until DCacheWays) {
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.r.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.r.addr := io.cacheOp.req.bits.index
      }
    }
    cacheOpShouldResp := true.B
  }
  // when (io.cacheOp_req_dup(4).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(4))) {
  //   for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
  //     val ecc_bank = ecc_banks(bank_index)
  //     ecc_bank.io.r.req.valid := true.B
  //     ecc_bank.io.r.req.bits.setIdx := io.cacheOp.req.bits.index
  //   }
  //   cacheOpShouldResp := true.B
  // }
  when(io.cacheOp_req_dup(5).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(5))) {
    for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
      for (way_index <- 0 until DCacheWays) {
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.w.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.w.addr := io.cacheOp.req.bits.index
        data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bank_index)
      }
    }
    cacheOpShouldResp := true.B
  }
  // when(io.cacheOp_req_dup(6).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(6))){
  //   for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
  //     val ecc_bank = ecc_banks(bank_index)
  //     ecc_bank.io.w.req.valid := true.B
  //     ecc_bank.io.w.req.bits.apply(
  //       setIdx = io.cacheOp.req.bits.index,
  //       data = io.cacheOp.req.bits.write_data_ecc,
  //       waymask = UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
  //     )
  //   }
  //   cacheOpShouldResp := true.B
  // }

  when(io.cacheOp_req_dup(7).valid && CacheInstrucion.isReadData(io.cacheOp_req_bits_opCode_dup(7))) {
    for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
      for (way_index <- 0 until DCacheWays) {
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.r.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.r.addr := io.cacheOp.req.bits.index
      }
    }
    cacheOpShouldResp := true.B
  }
  // when (io.cacheOp_req_dup(8).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(8))) {
  //   for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
  //     val ecc_bank = ecc_banks(bank_index)
  //     ecc_bank.io.r.req.valid := true.B
  //     ecc_bank.io.r.req.bits.setIdx := io.cacheOp.req.bits.index
  //   }
  //     cacheOpShouldResp := true.B
  // }
  when(io.cacheOp_req_dup(9).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(9))) {
    for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
      for (way_index <- 0 until DCacheWays) {
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.w.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.w.addr := io.cacheOp.req.bits.index
        data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bank_index)
      }
    }
    cacheOpShouldResp := true.B
  }
  // when(io.cacheOp_req_dup(10).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(10))){
  //   for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
  //     val ecc_bank = ecc_banks(bank_index)
  //     ecc_bank.io.w.req.valid := true.B
  //     ecc_bank.io.w.req.bits.apply(
  //       setIdx = io.cacheOp.req.bits.index,
  //       data = io.cacheOp.req.bits.write_data_ecc,
  //       waymask = UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
  //     )
  //   }
  //   cacheOpShouldResp := true.B
  // }

  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  for (bank_index <- 0 until DCacheBanks) {
    //todo: which way
    io.cacheOp.resp.bits.read_data_vec(bank_index) := 0.U
  }
  // eccReadResult(bank_index) := ecc_banks(bank_index).io.r.resp.data(RegNext(io.cacheOp.req.bits.wayNum(4, 0)))
  io.cacheOp.resp.bits.read_data_ecc := 0.U //Mux(io.cacheOp.resp.valid,
  //   eccReadResult(RegNext(io.cacheOp.req.bits.bank_num)),
  //   0.U
  // )
}