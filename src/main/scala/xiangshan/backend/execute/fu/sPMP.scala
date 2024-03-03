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

package xiangshan.backend.execute.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xs.utils.{ParallelPriorityMux}
import xiangshan.cache.mmu.TlbCmd

trait SPMPConst extends PMPConst

class SPMPBase(implicit p: Parameters) extends PMPBase {
  override def write_cfg_vec(mask: Vec[UInt], addr: Vec[UInt], index: Int)(cfgs: UInt): UInt = {
    val cfgVec = Wire(Vec(cfgs.getWidth/8, new PMPConfig))
    for (i <- cfgVec.indices) {
      val cfg_w_m_tmp = cfgs((i+1)*8-1, i*8).asUInt.asTypeOf(new PMPConfig)
      cfgVec(i) := cfg_w_m_tmp
      // filter NA4
      if (CoarserGrain) { cfgVec(i).a := Cat(cfg_w_m_tmp.a(1), cfg_w_m_tmp.a.orR) }
      when (cfgVec(i).na4_napot) {
        mask(index + i) := match_mask(cfgVec(i), addr(index + i))
      }
    }
    cfgVec.asUInt
  }
}

trait SPMPMethod extends SPMPConst {
  def spmp_init() : (Vec[UInt], Vec[UInt], Vec[UInt])
}

trait SPMPCheckMethod extends SPMPConst {
  def spmp_check(cmd: UInt, cfg: PMPConfig) = {
    val resp = Wire(new PMPRespBundle)
    resp.ld := TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd) && !cfg.r
    resp.st := (TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd)) && !cfg.w
    resp.instr := TlbCmd.isExec(cmd) && !cfg.x
    resp.mmio := false.B
    resp
  }

  def spmp_match_res(leaveHitMux: Boolean = false, valid: Bool = true.B)(
    addr: UInt,
    size: UInt,
    spmpEntries: Vec[PMPEntry],
    mode: UInt,
    lgMaxSize: Int,
    //tlbCsr: TlbCsrBundle
    sum: Bool
  ) = {
    val num = spmpEntries.size
    require(num == NumPMA)

    val passThrough = if (spmpEntries.isEmpty) true.B else (mode > 1.U)
    val spmpDefault = WireInit(0.U.asTypeOf(new PMPEntry()))
    spmpDefault.cfg.r := mode(1) === 1.U
    spmpDefault.cfg.w := mode(1) === 1.U
    spmpDefault.cfg.x := mode(1) === 1.U

    val match_vec = Wire(Vec(num+1, Bool()))
    val cfg_vec = Wire(Vec(num+1, new PMPEntry()))

    spmpEntries.zip(spmpDefault +: spmpEntries.take(num-1)).zipWithIndex.foreach{ case ((spmp, last_spmp), i) =>
      val is_match = spmp.is_match(addr, size, lgMaxSize, last_spmp)
      val ignore = passThrough
      val aligned = spmp.aligned(addr, size, lgMaxSize, last_spmp)

      val cur = WireInit(spmp)
      // check logic
      val spmpTTCfg = spmp_truth_table_match(spmp.cfg, mode, sum)
      cur.cfg.r := aligned && (spmpTTCfg.r || ignore)
      cur.cfg.w := aligned && (spmpTTCfg.w || ignore)
      cur.cfg.x := aligned && (spmpTTCfg.x || ignore)

      match_vec(i) := is_match
      cfg_vec(i) := cur
    }

    match_vec(num) := true.B
    cfg_vec(num) := spmpDefault

    if (leaveHitMux) {
      ParallelPriorityMux(match_vec.map(RegEnable(_, false.B, valid)), RegEnable(cfg_vec, valid))
    } else {
      ParallelPriorityMux(match_vec, cfg_vec)
    }
  }

  def spmp_truth_table_match(cfg: PMPConfig, mode: UInt, sum: Bool): PMPConfig = {
    val trueTable: Seq[(BitPat, UInt)] = Array(
      // S,R,W,X,Mode,SUM -> r,w,x (Mode: 1:S, 0:U)
      BitPat("b0000_??") -> "b000".U,

      BitPat("b0001_1?") -> "b000".U,
      BitPat("b0001_0?") -> "b001".U,

      BitPat("b0010_1?") -> "b110".U,
      BitPat("b0010_0?") -> "b100".U,

      BitPat("b0011_??") -> "b110".U,

      BitPat("b0100_10") -> "b000".U,
      BitPat("b0100_11") -> "b100".U,
      BitPat("b0100_0?") -> "b100".U,

      BitPat("b0101_10") -> "b000".U,
      BitPat("b0101_11") -> "b100".U,
      BitPat("b0101_0?") -> "b101".U,

      BitPat("b0110_10") -> "b000".U,
      BitPat("b0110_11") -> "b110".U,
      BitPat("b0110_0?") -> "b110".U,

      BitPat("b0111_10") -> "b000".U,
      BitPat("b0111_11") -> "b110".U,
      BitPat("b0111_0?") -> "b111".U,

      BitPat("b1000_0?") -> "b111".U, // reserved, U
      BitPat("b1000_1?") -> "b111".U, // S

      BitPat("b1001_1?") -> "b001".U,
      BitPat("b1001_0?") -> "b000".U,

      BitPat("b1010_??") -> "b001".U,

      BitPat("b1011_1?") -> "b101".U,
      BitPat("b1011_0?") -> "b001".U,

      BitPat("b1100_1?") -> "b100".U,
      BitPat("b1100_0?") -> "b000".U,

      BitPat("b1101_1?") -> "b101".U,
      BitPat("b1101_0?") -> "b000".U,

      BitPat("b1110_1?") -> "b110".U,
      BitPat("b1110_0?") -> "b000".U,

      BitPat("b1111_??") -> "b100".U
    )

    val cur = WireInit(cfg)
    val pat = Cat(cfg.s, cfg.r, cfg.w, cfg.x, mode(0), sum.asUInt)
    val rwx = Lookup[UInt](pat, 0.U(3.W), trueTable)

    cur.r := rwx(2)
    cur.w := rwx(1)
    cur.x := rwx(0)
    cur
  }
}