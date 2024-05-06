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

package xiangshan.frontend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xs.utils._
import xs.utils.mbist.MBISTPipeline
import xs.utils.perf.HasPerfLogging
import xs.utils.sram.SRAMTemplate

import scala.{Tuple2 => &}


trait FTBParams extends HasXSParameter with HasBPUConst {
  val numEntries = FtbSize
  val numWays    = FtbWays
  val numSets    = numEntries/numWays // 512
  val tagSize    = 20

  val TAR_STAT_SZ = 2
  def TAR_FIT = 0.U(TAR_STAT_SZ.W)
  def TAR_OVF = 1.U(TAR_STAT_SZ.W)
  def TAR_UDF = 2.U(TAR_STAT_SZ.W)

  def BR_OFFSET_LEN = 12
  def JMP_OFFSET_LEN = 20
}

//cut ftb_entry_mem area
class FTBEntry_FtqMem(implicit p: Parameters) extends XSBundle with FTBParams with BPUUtils {
  
  /** Slot information */
  val valid   = Bool()
  val offset  = UInt(log2Ceil(PredictWidth).W)
  val sharing = Bool()  // means is branch

  /** Jump type */ //TODO: compat with sharing
  val isCall      = Bool()
  val isRet       = Bool()
  val isJalr      = Bool()

  def isJal: Bool = !isJalr

  //def brValid: Bool  = this.valid && this.sharing
  def jmpValid: Bool = this.valid && !this.sharing

  def noEmptySlotForNewBr: Bool = this.valid
  def newBrCanNotInsert(offset: UInt): Bool = this.valid && this.offset < offset
  def brIsRecorded(offset: UInt): Bool = this.valid && this.sharing && this.offset === offset
  def getBrMaskByOffset(offset: UInt) : Bool = this.valid && this.sharing && this.offset <= offset
}

class FTBEntry(implicit p: Parameters) extends XSBundle with FTBParams with BPUUtils {
  
  /** Slot information */
  val valid   = Bool()
  val offset  = UInt(log2Ceil(PredictWidth).W)
  val tarStat = UInt(TAR_STAT_SZ.W)
  val lower   = UInt(JMP_OFFSET_LEN.W)
  val sharing = Bool()  // means is branch

  /** Partial Fall-Through Address */
  val carry       = Bool()
  val pftAddr     = UInt(log2Up(PredictWidth).W)

  /** Jump type */ //TODO: compat with sharing
  val isCall      = Bool()
  val isRet       = Bool()
  val isJalr      = Bool()

  val last_may_be_rvi_call = Bool()

  val alwaysTaken = Bool()

  def setLowerStatByTarget(pc: UInt, target: UInt, isShare: Boolean = false): Unit = {
    val pcHigher = pc(VAddrBits - 1, JMP_OFFSET_LEN + 1)
    val targetHigher = target(VAddrBits - 1, JMP_OFFSET_LEN + 1)
    val stat = Mux(targetHigher > pcHigher, TAR_OVF, Mux(targetHigher < pcHigher, TAR_UDF, TAR_FIT))
    val lower = target(JMP_OFFSET_LEN, 1)
    this.lower := lower
    this.tarStat := stat
    this.sharing := isShare.B
  }

  def getTarget(pc: UInt): UInt = {
    val h = pc(VAddrBits - 1, JMP_OFFSET_LEN + 1)
    val higher = MuxLookup(tarStat, h)(Seq(
      TAR_OVF -> (h + 1.U),
      TAR_UDF -> (h - 1.U),
      TAR_FIT -> h
    ))
    Cat(higher, lower, 0.U(1.W))
  }

  def getFallThrough(pc: UInt): UInt = {
    val higher = pc.head(VAddrBits - log2Ceil(PredictWidth) - instOffsetBits)
    Cat(Mux(carry, higher + 1.U, higher), pftAddr, 0.U(instOffsetBits.W))
  }

  def isJal: Bool = !isJalr

  def brValid: Bool  = this.valid && this.sharing
  def jmpValid: Bool = this.valid && !this.sharing

  def noEmptySlotForNewBr: Bool = this.valid
  def newBrCanNotInsert(offset: UInt): Bool = this.valid && this.offset < offset
  def brIsRecorded(offset: UInt): Bool = this.valid && this.sharing && this.offset === offset
  def getBrMaskByOffset(offset: UInt) : Bool = this.valid && this.sharing && this.offset <= offset
}

class FTBEntryWithTag(implicit p: Parameters) extends XSBundle with FTBParams with BPUUtils {
  val entry = new FTBEntry
  val tag = UInt(tagSize.W)
}

class FTBMeta(implicit p: Parameters) extends XSBundle with FTBParams {
  val writeWay = UInt(log2Ceil(numWays).W)
  val hit = Bool()
  val fauFtbHit = if (EnableFauFTB) Some(Bool()) else None
  val predCycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None
}

object FTBMeta {
  def apply(writeWay: UInt, hit: Bool, fauhit: Bool, predCycle: UInt)(implicit p: Parameters): FTBMeta = {
    val e = Wire(new FTBMeta)
    e.writeWay := writeWay
    e.hit := hit
    e.fauFtbHit.foreach(_ := fauhit)
    e.predCycle.foreach(_ := predCycle)
    e
  }
}

class FTB(parentName:String = "Unknown")(implicit p: Parameters) extends BasePredictor with FTBParams with BPUUtils
  with HasCircularQueuePtrHelper with HasPerfEvents {
  override val meta_size = WireInit(0.U.asTypeOf(new FTBMeta)).getWidth

  val ftbAddr = new TableAddr(log2Up(numSets), 1)

  class FTBBank(val numSets: Int, val nWays: Int) extends XSModule with BPUUtils with HasPerfLogging {
    val io = IO(new Bundle {
      val s1_fire = Input(Bool())

      /** When Ftb hit, readHits.valid is True and readHits.bits is OneHot of hit way.
       *  When Ftb miss, readHits.valid is False and readHits.bits is OneHot of allocation way.
       */
      /** Predict read */
      val reqPC = Flipped(DecoupledIO(UInt(VAddrBits.W)))
      val readResp = Output(new FTBEntry)
      val readHits = Valid(UInt(log2Ceil(numWays).W))
      /** Update read */
      val u_reqPC = Flipped(DecoupledIO(UInt(VAddrBits.W)))
      val updateHits = Valid(UInt(log2Ceil(numWays).W))
      val updateAccess = Input(Bool())
      /** Update write */
      val updatePC = Input(UInt(VAddrBits.W))
      val updateWriteDate = Flipped(Valid(new FTBEntryWithTag))
      val updateWriteWay = Input(UInt(log2Ceil(numWays).W))
      val updateWriteAlloc = Input(Bool())
    })

    // Extract holdRead logic to fix bug that update read override predict read result
    private val ftb = Module(new SRAMTemplate(new FTBEntryWithTag, set = numSets, way = numWays, shouldReset = true, holdRead = false, singlePort = true,
      hasMbist = coreParams.hasMbist,
      hasShareBus = coreParams.hasShareBus,
      parentName = parentName
    ))
    private val ftbReadEntries = ftb.io.r.resp.data.map(_.entry)

    private val predRdata = HoldUnless(ftb.io.r.resp.data, RegNext(io.reqPC.valid && !io.updateAccess))
    private val readEntries = predRdata.map(_.entry)
    private val readTags = predRdata.map(_.tag)

    ftb.io.r.req.valid := io.reqPC.valid || io.u_reqPC.valid // io.s0_fire
    ftb.io.r.req.bits.setIdx := Mux(io.u_reqPC.valid, ftbAddr.getIdx(io.u_reqPC.bits), ftbAddr.getIdx(io.reqPC.bits)) // s0_idx

    assert(!(io.reqPC.valid && io.u_reqPC.valid))

    io.reqPC.ready := ftb.io.r.req.ready
    io.u_reqPC.ready := ftb.io.r.req.ready

    val reqTag = RegEnable(ftbAddr.getTag(io.reqPC.bits)(tagSize-1, 0), io.reqPC.valid)
    val reqIdx = RegEnable(ftbAddr.getIdx(io.reqPC.bits), io.reqPC.valid)

    val u_reqTag = RegEnable(ftbAddr.getTag(io.u_reqPC.bits)(tagSize-1, 0), io.u_reqPC.valid)

    val totalHits: Vec[Bool] = VecInit((0 until numWays).map(b => readTags(b) === reqTag && readEntries(b).valid && io.s1_fire))
    val hit: Bool = totalHits.reduce(_||_)
    val hitWay: UInt = OHToUInt(totalHits)

    val u_totalHits = VecInit((0 until numWays).map(b =>
        ftb.io.r.resp.data(b).tag === u_reqTag && ftb.io.r.resp.data(b).entry.valid && RegNext(io.updateAccess)))
    val u_hit = u_totalHits.reduce(_||_)
    val u_hitWay = OHToUInt(u_totalHits)

    for (n <- 1 to numWays) {
      XSPerfAccumulate(f"ftb_pred_${n}_way_hit", PopCount(totalHits) === n.U)
      XSPerfAccumulate(f"ftb_update_${n}_way_hit", PopCount(u_totalHits) === n.U)
    }

    val replacer = ReplacementPolicy.fromString(Some("setplru"), numWays, numSets)

    val touchSet = Seq.fill(1)(Wire(UInt(log2Ceil(numSets).W)))
    val touchWay = Seq.fill(1)(Wire(Valid(UInt(log2Ceil(numWays).W))))

    val writeSet = Wire(UInt(log2Ceil(numSets).W))
    val writeWay = Wire(Valid(UInt(log2Ceil(numWays).W)))

    val readSet = Wire(UInt(log2Ceil(numSets).W))
    val readWay = Wire(Valid(UInt(log2Ceil(numWays).W)))

    readSet := reqIdx
    readWay.valid := hit
    readWay.bits  := hitWay

    touchSet.head := Mux(writeWay.valid, writeSet, readSet)

    touchWay.head.valid := writeWay.valid || readWay.valid
    touchWay.head.bits := Mux(writeWay.valid, writeWay.bits, readWay.bits)

    replacer.access(touchSet, touchWay)

    io.readResp := Mux1H(totalHits, readEntries)
    io.readHits.valid := hit
    io.readHits.bits := hitWay

    io.updateHits.valid := u_hit
    io.updateHits.bits := u_hitWay

    // Update logic
    val u_valid = io.updateWriteDate.valid
    val u_data = io.updateWriteDate.bits
    val u_idx = ftbAddr.getIdx(io.updatePC)
    val wayValids = VecInit(ftbReadEntries.map(_.valid)).asUInt
    val allocWriteWay = Mux(wayValids.andR, replacer.way(u_idx), PriorityEncoder(~wayValids))
    val u_way = Mux(io.updateWriteAlloc, allocWriteWay, io.updateWriteWay)
    val u_mask = UIntToOH(u_way)

    for (i <- 0 until numWays) {
      XSPerfAccumulate(f"ftb_replace_way$i", u_valid && io.updateWriteAlloc && u_way === i.U)
      XSPerfAccumulate(f"ftb_replace_way${i}_has_empty", u_valid && io.updateWriteAlloc && !ftbReadEntries.map(_.valid).reduce(_&&_) && u_way === i.U)
      XSPerfAccumulate(f"ftb_hit_way$i", hit && !io.updateAccess && hitWay === i.U)
    }

    ftb.io.w.apply(u_valid, u_data, u_idx, u_mask)

    // for replacer
    writeSet := u_idx
    writeWay.valid := u_valid
    writeWay.bits := Mux(io.updateWriteAlloc, allocWriteWay, io.updateWriteWay)

  } // FTBBank

  private val ftbBank = Module(new FTBBank(numSets, numWays))
  private val mbistPipeline = if (coreParams.hasMbist && coreParams.hasShareBus) {
    MBISTPipeline.PlaceMbistPipeline(1, s"${parentName}_mbistPipe")
  } else {
    None
  }

  ftbBank.io.reqPC.valid := io.s0_fire(dupForFtb)
  ftbBank.io.reqPC.bits := s0_pc_dup(dupForFtb)

  val btb_enable_dup = RegNext(dup(io.ctrl.btb_enable))
  val s2_ftbEntryDup = io.s1_fire.map(f => RegEnable(ftbBank.io.readResp, f))
  val s3_ftbEntryDup = io.s2_fire.zip(s2_ftbEntryDup).map {case (f, e) => RegEnable(e, f)}
  
  val s1_ftbHit = ftbBank.io.readHits.valid && io.ctrl.btb_enable
  val s1_uftbHitDup = io.in.bits.resp_in(0).s1.fullPred.map(_.hit)
  val s2_ftbHitDup = io.s1_fire.map(f => RegEnable(s1_ftbHit, f))
  val s2_uftbHitDup =
    if (EnableFauFTB) {
      io.s1_fire.zip(s1_uftbHitDup).map {case (f, h) => RegEnable(h, f)}
    } else {
      s2_ftbHitDup
    }
  val s2_realHitDup = s2_ftbHitDup.zip(s2_uftbHitDup).map(tp => tp._1 || tp._2)
  val s3_hitDup = io.s2_fire.zip(s2_realHitDup).map {case (f, h) => RegEnable(h, f)}
  val writeWay = ftbBank.io.readHits.bits

  io.out := io.in.bits.resp_in(0)

  val s1_latch_call_is_rvc   = DontCare // TODO: modify when add RAS

  io.out.s2.fullPred.zip(s2_realHitDup).foreach {case (fp, h) => fp.hit := h}
  val s2_uftbFullPredDup = io.s1_fire.zip(io.in.bits.resp_in(0).s1.fullPred).map {case (f, fp) => RegEnable(fp, f)}
  for (fullPred & s2_ftb_entry & s2_pc  & s2_uftbFullPred & s2_hit & s2_uftbHit <-
    io.out.s2.fullPred zip s2_ftbEntryDup zip s2_pc_dup zip s2_uftbFullPredDup zip s2_ftbHitDup zip s2_uftbHitDup) {
      if (EnableFauFTB) {
        // use uftb pred when ftb not hit but uftb hit
        when (!s2_hit && s2_uftbHit) {
          fullPred := s2_uftbFullPred
        }.otherwise {
          fullPred.fromFtbEntry(s2_ftb_entry, s2_pc)
        }
      } else {
        fullPred.fromFtbEntry(s2_ftb_entry, s2_pc)
      }
    }

  // s3
  val s3_fullPred = io.s2_fire.zip(io.out.s2.fullPred).map {case (f, fp) => RegEnable(fp, f)}
  // br_taken from SC in stage3 is covered here, will be recovered in always taken logic
  io.out.s3.fullPred := s3_fullPred

  val s3_fauftbHitFtbMiss = RegEnable(!s2_ftbHitDup(dupForFtb) && s2_uftbHitDup(dupForFtb), io.s2_fire(dupForFtb))
  io.out.lastStageFtbEntry := Mux(s3_fauftbHitFtbMiss, io.in.bits.resp_in(0).lastStageFtbEntry, s3_ftbEntryDup(dupForFtb))
  io.out.lastStageMeta := RegEnable(RegEnable(FTBMeta(writeWay.asUInt, s1_ftbHit, s1_uftbHitDup(dupForFtb), GTimer()).asUInt, io.s1_fire(dupForFtb)), io.s2_fire(dupForFtb))

  // always taken logic
  for (out_fp & in_fp & s2_hit & s2_ftb_entry <-
    io.out.s2.fullPred zip io.in.bits.resp_in(0).s2.fullPred zip s2_ftbHitDup zip s2_ftbEntryDup)
    out_fp.br_taken := in_fp.br_taken || s2_hit && s2_ftb_entry.alwaysTaken
  for (out_fp & in_fp & s3_hit & s3_ftb_entry <-
    io.out.s3.fullPred zip io.in.bits.resp_in(0).s3.fullPred zip s3_hitDup zip s3_ftbEntryDup)
    out_fp.br_taken := in_fp.br_taken || s3_hit && s3_ftb_entry.alwaysTaken


  // Update logic
  val u = io.update(dupForFtb)
  val update = u.bits

  val u_meta = update.meta.asTypeOf(new FTBMeta)
  // we do not update ftb on fauFtb hit and ftb miss
  val update_uftbHitFtbMiss = u_meta.fauFtbHit.getOrElse(false.B) && !u_meta.hit
  val u_valid = u.valid && !u.bits.oldEntry && !(update_uftbHitFtbMiss)

  val updateDelay2 = Pipe(u, 2)
  val delay2PC = updateDelay2.bits.pc
  val delay2Entry = updateDelay2.bits.ftbEntry

  
  val updateNow = u_valid && u_meta.hit
  val updateNeedRead = u_valid && !u_meta.hit
  // stall one more cycle because we use a whole cycle to do update read tag hit
  io.s1_ready := ftbBank.io.reqPC.ready && !(updateNeedRead) && !RegNext(updateNeedRead)

  ftbBank.io.u_reqPC.valid := updateNeedRead
  ftbBank.io.u_reqPC.bits := update.pc

  val ftbWrite = Wire(new FTBEntryWithTag)
  ftbWrite.entry := Mux(updateNow, update.ftbEntry, delay2Entry)
  ftbWrite.tag   := ftbAddr.getTag(Mux(updateNow, update.pc, delay2PC))(tagSize-1, 0)

  val writeValid = updateNow || DelayN(u_valid && !u_meta.hit, 2)

  ftbBank.io.updateWriteDate.valid := writeValid
  ftbBank.io.updateWriteDate.bits := ftbWrite
  ftbBank.io.updatePC         := Mux(updateNow, update.pc,       delay2PC)
  ftbBank.io.updateWriteWay   := Mux(updateNow, u_meta.writeWay, RegNext(ftbBank.io.updateHits.bits)) // use it one cycle later
  ftbBank.io.updateWriteAlloc := Mux(updateNow, false.B,         RegNext(!ftbBank.io.updateHits.valid)) // use it one cycle later
  ftbBank.io.updateAccess := u_valid && !u_meta.hit
  ftbBank.io.s1_fire := io.s1_fire(dupForFtb)

  XSDebug("req_v=%b, reqPC=%x, ready=%b (resp at next cycle)\n", io.s0_fire(dupForFtb), s0_pc_dup(dupForFtb), ftbBank.io.reqPC.ready)
  XSDebug("s2_hit=%b, hitWay=%b\n", s2_ftbHitDup(dupForFtb), writeWay.asUInt)
  XSDebug("s2_br_taken_mask=%b, s2_real_taken_mask=%b\n",
    io.in.bits.resp_in(dupForFtb).s2.fullPred(dupForFtb).br_taken, io.out.s2.fullPred(dupForFtb).realSlotTaken)
  XSDebug("s2_target=%x\n", io.out.s2.target(dupForFtb))

  XSPerfAccumulate("ftb_read_hits", RegNext(io.s0_fire(dupForFtb)) && s1_ftbHit)
  XSPerfAccumulate("ftb_read_misses", RegNext(io.s0_fire(dupForFtb)) && !s1_ftbHit)

  XSPerfAccumulate("ftb_commit_hits", u.valid && u_meta.hit)
  XSPerfAccumulate("ftb_commit_misses", u.valid && !u_meta.hit)

  XSPerfAccumulate("ftb_update_req", u.valid)

  XSPerfAccumulate("ftb_update_ignored_old_entry", u.valid && u.bits.oldEntry)
  XSPerfAccumulate("ftb_update_ignored_fauftb_hit", u.valid && update_uftbHitFtbMiss)
  XSPerfAccumulate("ftb_updated", u_valid)

  override val perfEvents = Seq(
    ("ftb_commit_hits            ", u.valid  &&  u_meta.hit),
    ("ftb_commit_misses          ", u.valid  && !u_meta.hit),
  )
  generatePerfEvent()
}
