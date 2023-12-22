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

import chisel3._
import chisel3.util._
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegReadFn, RegWriteFn}
import xiangshan.cache.mmu.TlbCmd
import xs.utils.{ParallelPriorityMux, ValidHold, ZeroExt}

import scala.collection.mutable.ListBuffer

/* Memory Mapped PMA */
case class MMPMAConfig
(
  address: BigInt,
  mask: BigInt,
  lgMaxSize: Int,
  sameCycle: Boolean,
  num: Int
)

trait PMAConst extends PMPConst

trait MMPMAMethod extends PMAConst with PMAMethod with PMPReadWriteMethodBare {
  def gen_mmpma_mapping(num: Int) = {
    val pmaCfgPerCSR = PMXLEN / new PMPConfig().getWidth
    def pmaCfgLogicIndex(i: Int) = (PMXLEN / 32) * (i / pmaCfgPerCSR)
    def pmaCfgIndex(i: Int) = (i / pmaCfgPerCSR)

    val pma = Wire(Vec(num, new PMPEntry))

    /* pma init value */
    val init_value = pma_init()

    val pmaCfgMerged = RegInit(init_value._1)
    val addr = RegInit(init_value._2)
    val mask = RegInit(init_value._3)
    val cfg = WireInit(pmaCfgMerged).asTypeOf(Vec(num, new PMPConfig()))
    //  pmaMask are implicit regs that just used for timing optimization
    for (i <- pma.indices) {
      pma(i).gen(cfg(i), addr(i), mask(i))
    }

    val blankCfg = PMXLEN == 32
    val cfg_index_wrapper = (0 until num by 4).zip((0 until num by 4).map(a => blankCfg || (a % pmaCfgPerCSR == 0)))
    val cfg_map = (cfg_index_wrapper).map{ case(i, notempty) => {
//      println(s"tlbpma i:$i notempty:$notempty")
      RegField.apply(n = PMXLEN, r = RegReadFn{(ivalid, oready) =>
        val r_ready = Wire(Bool())
        val o_valid = Wire(Bool())
        val v_reg = ValidHold(r_ready && ivalid, o_valid && oready, false.B)
        r_ready := !v_reg
        o_valid := v_reg

        if (notempty) { (r_ready, o_valid, pmaCfgMerged(pmaCfgIndex(i))) }
        else { (r_ready, o_valid, 0.U) }
      }, w = RegWriteFn((valid, data) => {
        if (notempty) { when (valid) { pmaCfgMerged(pmaCfgIndex(i)) := write_cfg_vec(mask, addr, i)(data) } }
        true.B
      }), desc = RegFieldDesc(s"MMPMA_config_${i}", s"pma config register #${i}"))
    }}

    val addr_map = (0 until num).map{ i => {
      val next_cfg = if (i == 0) 0.U.asTypeOf(new PMPConfig()) else cfg(i-1)
      RegField(
        n = PMXLEN,
        r = ZeroExt(read_addr(cfg(i))(addr(i)), PMXLEN),
        w = RegWriteFn((valid, data) => {
          when (valid) { addr(i) := write_addr(next_cfg, mask(i))(data(addr(0).getWidth-1, 0), cfg(i), addr(i))}
          true.B
        }),
        desc = RegFieldDesc(s"MMPMA_addr_${i}", s"pma addr register #${i}")
      )
    }}

    (cfg_map, addr_map, pma)
  }

}

trait PMAMethod extends PMAConst {
  /**
   * from  CPU
   * BASE            TOP             Size   Description       Attribute
   * 0x000_0000_0000 0x000_0001_FFFF 128KB  RC_ROM            RW
   * 0x000_0002_0000 0x000_0003_FFFF 128KB  XS_ROM            RX
   * 0x000_0004_0000 0x000_0004_0FFF 4KB    GPIO              RW
   * 0x000_0005_0000 0x000_0005_0FFF 4KB    UART0             RW
   * 0x000_0005_1000 0x000_0005_1FFF 4KB    UART1             RW
   * 0x000_0006_0000 0x000_0006_0FFF 4KB    I2C_MASTER_0      RW
   * 0x000_0006_1000 0x000_0006_1FFF 4KB    I2C_MASTER_1      RW
   * 0x000_0007_0000 0x000_0007_03FF 1KB    timer_riscv       RW
   * 0x000_0007_1000 0x000_0007_13FF 1KB    timer0-timer7     RW
   * 0x000_0008_0000 0x000_0008_0FFF 4KB    wdt               RW
   * 0x000_0008_1000 0x000_0008_1FFF 4KB    ssi_master qspi_1 RW
   * 0x000_0008_2000 0x000_0008_2FFF 4KB    ssi_master ospi   RW
   * 0x000_0009_0000 0x000_0408_FFFF 64MB   ssi_master qspi_0 RWX
   * 0x000_0410_1000 0x000_0410_1FFF 4KB    peri1_cru         RW
   * 0x000_0410_2000 0x000_0410_2FFF 4KB    soc_cru           RW
   * 0x000_0410_3000 0x000_0410_3FFF 4KB    io_cfg            RW
   * 0x000_0410_4000 0x000_0410_4FFF 4KB    core_cru          RW
   * 0x000_0410_5000 0x000_0410_5FFF 4KB    pwr_ctrl          RW
   * 0x000_0411_0000 0x000_0411_FFFF 64KB   dma_cfg           RW
   * 0x000_0412_0000 0x000_0412_03FF 1KB    pvt sensor        RW
   * 0x000_0412_4000 0x000_0412_4FFF 4KB    pcie_cru          RW
   * 0x000_0412_5000 0x000_0412_5FFF 4KB    ddr_cru           RW
   * 0x000_0420_0000 0x000_042F_FFFF 1MB    RC_cfg            RW
   * 0x000_0500_0000 0x000_05FF_FFFF 16MB   pcie_cfg          RW
   * 0x000_0640_0000 0x000_067F_FFFF 4MB    iommu_dma_cfg     RW
   * 0x000_0680_0000 0x000_06CF_FFFF 4MB    iommu_pcie_cfg    RW
   * 0x000_0700_0000 0x000_07FF_FFFF 16MB   ddr_phy_cfg       RW
   * 0x000_0800_0000 0x000_0800_FFFF 64KB   ddr_ctrl_cfg      RW
   * 0x000_3800_0000 0x000_3800_FFFF 64KB   xs_clint          RW
   * 0x000_3801_0000 0x000_3801_FFFF 64KB   xs_beu            RW
   * 0x000_3802_0000 0x000_3802_0FFF 4KB    xs_debugmodule    RW
   * 0x000_3900_0000 0x000_3900_0FFF 4KB    xs_l3cachectrl    RW
   * 0x000_3C00_0000 0x000_3FFF_FFFF 64MB   xs_plic           RW
   * 0x000_4000_0000 0x000_5FFF_FFFF 512MB  PCIE_X8           RW
   * 0x000_6000_0000 0x000_7FFF_FFFF 512MB  PCIE_X4           RW
   * 0x000_8000_0000 0x000_9FFF_FFFF 512MB  PCIE_X2           RW
   * 0x000_A000_0000 0x000_BFFF_FFFF 512MB  PCIE_X2           RW
   * 0x000_C000_0000 0x000_FFFF_FFFF 1GB    PCIE_X4           RW
   * 0x001_0000_0000 0x001_7FFF_FFFF 2GB    PCIE_X8           RW
   * 0x001_8000_0000 0x001_BFFF_FFFF 1GB    PCIE_X2           RW
   * 0x001_C000_0000 0x001_FFFF_FFFF 1GB    PCIE_X2           RW
   * 0x002_0000_0000 0x008_FFFF_FFFF 28GB   PCIE_X8           RW
   * 0x009_0000_0000 0x00D_FFFF_FFFF 20GB   PCIE_X4           RW
   * 0x00E_0000_0000 0x00E_FFFF_FFFF 4GB    PCIE_X2           RW
   * 0x00F_0000_0000 0x00F_FFFF_FFFF 4GB    PCIE_X2           RW
   * 0x010_0000_0000 0x01F_FFFF_FFFF 64GB   DDR               RWXIDSA
   */

  def pma_init() : (Vec[UInt], Vec[UInt], Vec[UInt]) = {
    def genAddr(init_addr: BigInt) = {
      init_addr.U((PMPAddrBits - PMPOffBits).W)
    }
    def genMask(init_addr: BigInt, a: BigInt) = {
      val match_mask_addr = (init_addr << 1) | (a & 0x1) | (((1 << PlatformGrain) - 1) >> PMPOffBits)
      val mask = ((match_mask_addr & ~(match_mask_addr + 1)) << PMPOffBits) | ((1 << PMPOffBits) - 1)
      mask.U(PMPAddrBits.W)
    }

    val num = NumPMA
    require(num >= 16)

    val cfg_list = ListBuffer[UInt]()
    val addr_list = ListBuffer[UInt]()
    val mask_list = ListBuffer[UInt]()
    def addPMA(base_addr: BigInt,
               range: BigInt = 0L, // only use for napot mode
               l: Boolean = false,
               c: Boolean = false,
               atomic: Boolean = false,
               a: Int = 0,
               x: Boolean = false,
               w: Boolean = false,
               r: Boolean = false) = {
      val addr = if (a < 2) { shift_addr(base_addr) }
        else { get_napot(base_addr, range) }
      cfg_list.append(PMPConfigUInt(l, c, atomic, a, x, w, r))
      addr_list.append(genAddr(addr))
      mask_list.append(genMask(addr, a))
    }

    addPMA(0x0L, 0x2000000000L, c = true, atomic = true, a = 3, x = true, w = true, r = true)
    addPMA(0x1FFF800000L, a = 1, c = true, atomic = true, w = true, r = true, x = true)
    addPMA(0x1000000000L, a = 1, w = true, r = true)
    addPMA(0x3b300000L, a = 1, x = true, r = true)
    addPMA(0x3b1f0000L, a = 1, w = true, r = true)
    addPMA(0x0039000000L, a = 1)
    addPMA(0x0038021000L, a = 1, w = true, r = true, x = true)
    addPMA(0x0038020000L, a = 1, w = true, r = true)
    addPMA(0x0037000000L, a = 1)
    addPMA(0x000A800000L, a = 1, w = true, r = true)
    addPMA(0x0004200000L, a = 1)
    addPMA(0x0004126000L, a = 1, w = true, r = true, x = true)
    addPMA(0x0000090000L, a = 1)
    addPMA(0x0000082000L, a = 1, w = true, r = true)
    addPMA(0x0000040000L, a = 1, r = true, x = true)
    addPMA(0)
    while (cfg_list.length < NumPMA) {
      addPMA(0)
    }

    val cfgInitMerge = Seq.tabulate(num / 8)(i => {
      cfg_list.reverse.drop(8 * i).take(8).foldRight(BigInt(0L)) { case (a, result) =>
        (result << a.getWidth) | a.litValue
      }.U(PMXLEN.W)
    })
    val addr = addr_list.reverse.toSeq
    val mask = mask_list.reverse.toSeq
    (VecInit(cfgInitMerge), VecInit(addr), VecInit(mask))
  }

  def get_napot(base: BigInt, range: BigInt): BigInt = {
    val PlatformGrainBytes = (1 << PlatformGrain)
    if ((base % PlatformGrainBytes) != 0) {
      println("base:%x", base)
    }
    if ((range % PlatformGrainBytes) != 0) {
      println("range: %x", range)
    }
    require((base % PlatformGrainBytes) == 0)
    require((range % PlatformGrainBytes) == 0)

    ((base + (range/2 - 1)) >> PMPOffBits)
  }

  def match_mask(paddr: UInt, cfg: PMPConfig) = {
    val match_mask_addr: UInt = Cat(paddr, cfg.a(0)).asUInt | (((1 << PlatformGrain) - 1) >> PMPOffBits).U((paddr.getWidth + 1).W)
    Cat(match_mask_addr & ~(match_mask_addr + 1.U), ((1 << PMPOffBits) - 1).U(PMPOffBits.W))
  }

  def shift_addr(addr: BigInt) = {
    addr >> 2
  }
}

trait PMACheckMethod extends PMAConst {
  def pma_check(cmd: UInt, cfg: PMPConfig) = {
    val resp = Wire(new PMPRespBundle)
    resp.ld := TlbCmd.isRead(cmd) && (TlbCmd.isAtom(cmd) && !cfg.atomic || !TlbCmd.isAtom(cmd) && !cfg.r)
    resp.st := TlbCmd.isWrite(cmd) && (!TlbCmd.isAmo(cmd) && !cfg.w || TlbCmd.isAtom(cmd) && !cfg.atomic)
    resp.instr := TlbCmd.isExec(cmd) && !cfg.x
    resp.mmio := !cfg.c
    resp
  }

  def pma_match_res(leaveHitMux: Boolean = false, valid: Bool = true.B)(
    addr: UInt,
    size: UInt,
    pmaEntries: Vec[PMPEntry],
    mode: UInt,
    lgMaxSize: Int
  ) = {
    val num = pmaEntries.size
    require(num == NumPMA)
    // pma should always be checked, could not be ignored
    // like amo and cached, it is the attribute not protection
    // so it must have initialization.
    require(!pmaEntries.isEmpty)

    val pmaDefault = WireInit(0.U.asTypeOf(new PMPEntry()))
    val match_vec = Wire(Vec(num+1, Bool()))
    val cfg_vec = Wire(Vec(num+1, new PMPEntry()))

    pmaEntries.zip(pmaDefault +: pmaEntries.take(num-1)).zipWithIndex.foreach{ case ((pma, last_pma), i) =>
      val is_match = pma.is_match(addr, size, lgMaxSize, last_pma)
      val aligned = pma.aligned(addr, size, lgMaxSize, last_pma)

      val cur = WireInit(pma)
      cur.cfg.r := aligned && pma.cfg.r
      cur.cfg.w := aligned && pma.cfg.w
      cur.cfg.x := aligned && pma.cfg.x
      cur.cfg.atomic := aligned && pma.cfg.atomic
      cur.cfg.c := aligned && pma.cfg.c

      match_vec(i) := is_match
      cfg_vec(i) := cur
    }

    match_vec(num) := true.B
    cfg_vec(num) := pmaDefault
    if (leaveHitMux) {
      ParallelPriorityMux(match_vec.map(RegEnable(_, false.B, valid)), RegEnable(cfg_vec, valid))
    } else {
      ParallelPriorityMux(match_vec, cfg_vec)
    }
  }
}
