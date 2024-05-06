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
import utils._
import xs.utils._
import xiangshan._
import scala.{Tuple2 => &}

trait FauFTBParams extends HasXSParameter with HasBPUConst {
  val numWays = 64
  val tagSize = 16

  def getTag(pc: UInt): UInt = pc(tagSize + instOffsetBits - 1, instOffsetBits)
}

class FauFTBWay(implicit p: Parameters) extends XSModule with FauFTBParams {
  val io = IO(new Bundle{
    /** Predict read */
    val reqTag  = Input(UInt(tagSize.W))
    val resp    = Output(new FTBEntry)
    val respHit = Output(Bool())

    /** Update read */
    val updateReqTag = Input(UInt(tagSize.W))
    val updateHit    = Output(Bool())
    val tagRead      = Output(UInt(tagSize.W))

    /** Update write */
    val writeValid = Input(Bool())
    val writeEntry = Input(new FTBEntry)
    val writeTag   = Input(UInt(tagSize.W))
  })

  private val valid = RegInit(false.B)
  private val tag   = Reg(UInt(tagSize.W))
  private val data  = Reg(new FTBEntry)


  io.resp := data
  io.respHit := tag === io.reqTag && valid

  /** Write bypass to avoid multiple hit */
  io.updateHit := ((tag === io.updateReqTag) && valid) || ((io.writeTag === io.updateReqTag) && io.writeValid)
  io.tagRead := tag

  when (io.writeValid) {
    valid := true.B
    tag   := io.writeTag
    data  := io.writeEntry
  }
}

class FauFTBBank(implicit p: Parameters) extends XSModule with FauFTBParams {
  val io = IO(new Bundle {
    val reqTag = Input(UInt(tagSize.W))
    val respHitOH = Output(Vec(numWays, Bool()))
    val respEntries = Output(Vec(numWays, new FTBEntry))
    val respCtrs = Output(Vec(numWays, UInt(2.W)))

    val updateReqTag = Input(UInt(tagSize.W))
    val updateHitOH = Output(Vec(numWays, Bool()))

    val writeValidOH = Input(Vec(numWays, Bool()))
    val writeTag = Input(UInt(tagSize.W))
    val writeEntry = Input(new FTBEntry)

    val writeCtrsValid = Input(Vec(numWays, Bool()))
    val writeCtrs = Input(Vec(numWays, UInt(2.W)))
  })

  private val ways = Seq.fill(numWays)(Module(new FauFTBWay))
  private val ctrs = Seq.fill(numWays)(RegInit(2.U(2.W)))

  /** Predict Require */
  ways.foreach(_.io.reqTag := io.reqTag)

  /** Predict Response */
  io.respHitOH := ways.map(_.io.respHit)
  io.respEntries := ways.map(_.io.resp)
  io.respCtrs := ctrs

  /** Update Require */
  ways.foreach(_.io.updateReqTag := io.updateReqTag)
  io.updateHitOH := VecInit(ways.map(_.io.updateHit))

  /** Update write tag and entry */
  ways.zip(io.writeValidOH).foreach { case (w, v) => w.io.writeValid := v }
  ways.foreach(_.io.writeTag := io.writeTag)
  ways.foreach(_.io.writeEntry := io.writeEntry)

  /** Update write counter */
  for (ctr & valid & writeCtr <- ctrs zip io.writeCtrsValid zip io.writeCtrs) {
    when(valid) {
      ctr := writeCtr
    }
  }
}

class FauFTB(implicit p: Parameters) extends BasePredictor with FauFTBParams {

  private val fauftbEnable = RegNext(dup(io.ctrl.ubtb_enable))

  class FauFTBMeta(implicit p: Parameters) extends XSBundle with FauFTBParams {
    //val predWay = UInt(log2Ceil(numWays).W)
    val pred_way = if (!env.FPGAPlatform) Some(UInt(log2Ceil(numWays).W)) else None
    val hit = Bool()
  }
  val respMeta = Wire(new FauFTBMeta)
  override val meta_size = respMeta.getWidth
  override val is_fast_pred = true

  
  private val banks = Module(new FauFTBBank)
  dontTouch(banks.io)
  
  private val replacer = ReplacementPolicy.fromString("plru", numWays)
  private val replacerTouchWays = Wire(Vec(2, Valid(UInt(log2Ceil(numWays).W))))

  private val s1_fire = io.s1_fire(dupForUbtb)
  private val s1_pc = s1_pc_dup(dupForUbtb)

  /** Prediction
   * FauFtb's bank prediction require by pc only,
   * response all entries, hit one-hot and counters
   * which construct the stage one of [[FullBranchPrediction]].
   */
  banks.io.reqTag := getTag(s1_pc_dup(dupForUbtb))

  val s1_hitOH: UInt = banks.io.respHitOH.asUInt
  val s1_hit: Bool = s1_hitOH.orR
  val s1_hitWay: UInt = OHToUInt(s1_hitOH)
  val s1_predCandidate: Vec[FullBranchPrediction] = Wire(Vec(numWays, new FullBranchPrediction))
  
  banks.io.respCtrs zip banks.io.respEntries zip s1_predCandidate foreach { case((ctr, entry), pred) =>
    pred.hit := DontCare
    pred.br_taken := ctr(1) || entry.alwaysTaken
    pred.fromFtbEntry(entry, s1_pc)
  }

  val s1_hitPred: FullBranchPrediction = Mux1H(s1_hitOH, s1_predCandidate)
  
  XSError(PopCount(s1_hitOH) > 1.U, "fauFtb has multiple hits!\n")
  
  io.out.s1.fullPred.foreach(_ := s1_hitPred)
  io.out.s1.fullPred.zip(fauftbEnable).foreach {case (fp, en) => fp.hit := s1_hit && en}

  respMeta.hit := RegEnable(RegEnable(s1_hit, io.s1_fire(dupForUbtb)), io.s2_fire(dupForUbtb))
  //respMeta.predWay := RegEnable(RegEnable(s1_hitWay, io.s1_fire(dupForUbtb)), io.s2_fire(dupForUbtb))
  if(respMeta.pred_way.isDefined) {respMeta.pred_way.get := RegEnable(RegEnable(s1_hitWay, io.s1_fire(0)), io.s2_fire(0))}
  io.out.lastStageMeta := respMeta.asUInt

  private val s1_ftbEntry = Mux1H(s1_hitOH, banks.io.respEntries)
  io.out.lastStageFtbEntry := RegEnable(RegEnable(s1_ftbEntry, io.s1_fire(dupForUbtb)), io.s2_fire(dupForUbtb))

  for (i <- 1 until numDup) {
    XSError(io.out.s1.fullPred(i).asUInt =/= io.out.s1.fullPred(0).asUInt,
      p"fauFtb s1 pred $i differs from pred 0\n")
  }

  /** Update
   * Two-cycle consumption at update.
   * First cycle: read fauFtb entries and compares tag.
   * Second cycle: write the entries to the hit way or alloc way and update counter.
   * */

  // s0
  private val us = io.update(dupForUbtb)
  private val u_meta = us.bits.meta.asTypeOf(new FauFTBMeta)
  private val u_s0_tag = getTag(us.bits.pc)

  banks.io.updateReqTag := u_s0_tag
  private val u_s0_hitOH = banks.io.updateHitOH.asUInt
  private val u_s0_hit = u_s0_hitOH.orR
  private val u_s0_brUpdateValid = us.bits.ftbEntry.brValid && us.valid && !us.bits.ftbEntry.alwaysTaken

  // s1
  private val u_s1_valid    = RegNext(dup(us.valid, numWays + 1)) // duplicate for replacer
  private val u_s1_tag      = RegEnable(u_s0_tag, us.valid)
  private val u_s1_hitOH    = RegEnable(u_s0_hitOH, us.valid)
  private val u_s1_hit      = RegEnable(u_s0_hit, us.valid)
  private val u_s1_ftbEntry = RegEnable(us.bits.ftbEntry, us.valid)
  private val u_s1_brTaken  = RegEnable(us.bits.br_taken, us.valid)
  private val u_s1_brUpdateValid = RegEnable(u_s0_brUpdateValid, us.valid)

  private val u_s1_allocWay   = replacer.way
  private val u_s1_writeWayOH = Mux(u_s1_hit, u_s1_hitOH, UIntToOH(u_s1_allocWay))
  private val u_s1_writeValidOH = u_s1_writeWayOH.asBools.zipWithIndex.map{ case (way, i) => way && u_s1_valid(i) }

  for (w <- 0 until numWays) {
    banks.io.writeValidOH(w)  := u_s1_writeValidOH(w)
    banks.io.writeTag         := u_s1_tag
    banks.io.writeEntry       := u_s1_ftbEntry

    banks.io.writeCtrsValid(w) := u_s1_brUpdateValid && u_s1_writeValidOH(w)
    banks.io.writeCtrs(w)      := satUpdate(banks.io.respCtrs(w), len = 2, u_s1_brTaken)
  }

  /** Replacer
   * Replacer will touch the ways which prediction hit and update write.
   * Replacer will give the eviction way to [[u_s1_allocWay]]
   */
  replacerTouchWays(0).valid := RegNext(s1_fire && s1_hit)
  replacerTouchWays(0).bits  := RegEnable(s1_hitWay, s1_fire && s1_hit)
  replacerTouchWays(1).valid := u_s1_valid.last
  replacerTouchWays(1).bits  := OHToUInt(u_s1_writeWayOH)
  replacer.access(replacerTouchWays)

  /** Performance counters */
  val s0_fire_next_cycle = RegNext(io.s0_fire(dupForUbtb))
  val u_pred_hit_way_map   = (0 until numWays).map(w => s0_fire_next_cycle && s1_hit && s1_hitWay === w.U)
  //val u_commit_hit_way_map = (0 until numWays).map(w => us.valid && u_meta.hit && u_meta.predWay === w.U)

  if(u_meta.pred_way.isDefined) {
    val u_commit_hit_way_map = (0 until numWays).map(w => us.valid && u_meta.hit && u_meta.pred_way.get === w.U)
    for (w <- 0 until numWays) {
      XSPerfAccumulate(f"uftb_commit_hit_way_${w}", u_commit_hit_way_map(w))
    }
  }

  XSPerfAccumulate("uftb_read_hits",   s0_fire_next_cycle &&  s1_hit)
  XSPerfAccumulate("uftb_read_misses", s0_fire_next_cycle && !s1_hit)
  XSPerfAccumulate("uftb_commit_hits",   us.valid &&  u_meta.hit)
  XSPerfAccumulate("uftb_commit_misses", us.valid && !u_meta.hit)
  XSPerfAccumulate("uftb_commit_read_hit_pred_miss", us.valid && !u_meta.hit && u_s0_hitOH(0).orR)
  for (w <- 0 until numWays) {
    XSPerfAccumulate(f"uftb_pred_hit_way_${w}",   u_pred_hit_way_map(w))
    //XSPerfAccumulate(f"uftb_commit_hit_way_${w}", u_commit_hit_way_map(w))
    XSPerfAccumulate(f"uftb_replace_way_${w}", !u_s1_hit(0) && u_s1_allocWay(0) === w.U)
  }

  override val perfEvents = Seq(
    ("fauftb_commit_hit       ", us.valid &&  u_meta.hit),
    ("fauftb_commit_miss      ", us.valid && !u_meta.hit),
  )
  generatePerfEvent()
  
}
