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

import scala.math.min
import scala.{Tuple2 => &}

trait HasSCParameter extends TageParams {
}
// class TageReq(implicit p: Parameters) extends TageBundle {
//   val pc = UInt(VAddrBits.W)
//   val ghist = UInt(HistoryLength.W)
//   val foldedHist = new AllFoldedHistories(foldedGHistInfos)
// }
class SCReq(implicit p: Parameters) extends TageReq

abstract class SCBundle(implicit p: Parameters) extends TageBundle with HasSCParameter {}
abstract class SCModule(implicit p: Parameters) extends TageModule with HasSCParameter {}


class SCMeta(val ntables: Int)(implicit p: Parameters) extends XSBundle with HasSCParameter {
  val tageTakens = Bool()
  val scUsed = Bool()
  val scPreds = Bool()
  // Suppose ctrbits of all tables are identical
  val ctrs = Vec(ntables, SInt(SCCtrBits.W))
}


class SCResp(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val ctrs = Vec(2, SInt(ctrBits.W))
}

class SCUpdate(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val pc = UInt(VAddrBits.W)
  val foldedHist = new AllFoldedHistories(foldedGHistInfos)
  val mask = Bool()
  val oldCtrs = SInt(ctrBits.W)
  val tagePreds = Bool()
  val takens = Bool()
}

class SCTableIO(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val req = Input(Valid(new SCReq))
  val resp = Output(new SCResp(ctrBits))
  val update = Input(new SCUpdate(ctrBits))
}


class SCTable(val nRows: Int, val ctrBits: Int, val histLen: Int, parentName:String = "Unknown")(implicit p: Parameters)
  extends SCModule with HasFoldedHistory with HasPerfLogging {
  val io = IO(new SCTableIO(ctrBits))

  // val table = Module(new SRAMTemplate(SInt(ctrBits.W), set=nRows, way=2*TageBanks, shouldReset=true, holdRead=true, singlePort=false))
  val table = Module(new SRAMTemplate(SInt(ctrBits.W), set=nRows, way=2, shouldReset=true, holdRead=true, singlePort=false, bypassWrite=true,
    hasMbist = coreParams.hasMbist,
    hasShareBus = coreParams.hasShareBus,
    parentName = parentName + "table_"
  ))
  val mbistPipeline = if(coreParams.hasMbist && coreParams.hasShareBus) {
    MBISTPipeline.PlaceMbistPipeline(1, s"${parentName}_mbistPipe", true)
  } else {
    None
  }

  // def getIdx(hist: UInt, pc: UInt) = {
  //   (compute_folded_ghist(hist, log2Ceil(nRows)) ^ (pc >> instOffsetBits))(log2Ceil(nRows)-1,0)
  // }


  val idxFhInfo = (histLen, min(log2Ceil(nRows), histLen))

  def getFoldedHistoryInfo = Set(idxFhInfo).filter(_._1 > 0)

  def getIdx(pc: UInt, allFh: AllFoldedHistories): UInt = {
    if (histLen > 0) {
      val idx_fh = allFh.getHistWithInfo(idxFhInfo).foldedHist
      // require(idx_fh.getWidth == log2Ceil(nRows))
      ((pc >> instOffsetBits).asUInt ^ idx_fh)(log2Ceil(nRows) - 1, 0)
    }
    else {
      (pc >> instOffsetBits)(log2Ceil(nRows) - 1, 0)
    }
  }


  def ctrUpdate(ctr: SInt, cond: Bool): SInt = signedSatUpdate(ctr, ctrBits, cond)

  val s0_idx = getIdx(io.req.bits.pc, io.req.bits.foldedHist)
  val s1_idx = RegEnable(s0_idx, io.req.valid)

  val s1_pc = RegEnable(io.req.bits.pc, io.req.fire)
  val s1_unhashed_idx = s1_pc >> instOffsetBits

  table.io.r.req.valid := io.req.valid
  table.io.r.req.bits.setIdx := s0_idx

  val per_br_ctrs = table.io.r.resp.data

  io.resp.ctrs := per_br_ctrs

  val update_wdata = Wire(SInt(ctrBits.W)) // correspond to physical bridx
  val update_wdata_packed = VecInit(Seq.fill(2)(update_wdata))
  val updateWayMask = Wire(Vec(2, Bool())) // correspond to physical bridx

  val update_unhashed_idx = io.update.pc >> instOffsetBits
  updateWayMask(0) := io.update.mask && !io.update.tagePreds
  updateWayMask(1) := io.update.mask && io.update.tagePreds

  val update_idx = getIdx(io.update.pc, io.update.foldedHist)

  table.io.w.apply(
    valid = io.update.mask,
    data = update_wdata_packed,
    setIdx = update_idx,
    waymask = updateWayMask.asUInt
  )

  val wrBypassEntries = 16

  // let it corresponds to logical brIdx
  val wrbypasses = Module(new WrBypass(SInt(ctrBits.W), wrBypassEntries, log2Ceil(nRows), numWays=2))

  val ctrPos = io.update.tagePreds
  val bypass_ctr = wrbypasses.io.hit_data(ctrPos)
  val previous_ctr = io.update.oldCtrs
  val hit_and_valid = wrbypasses.io.hit && bypass_ctr.valid
  val oldCtr = Mux(hit_and_valid, bypass_ctr.bits, previous_ctr)
  val taken = io.update.takens
  update_wdata := ctrUpdate(oldCtr, taken)

  wrbypasses.io.wen := io.update.mask
  wrbypasses.io.write_idx := update_idx
  wrbypasses.io.write_data := update_wdata_packed
  wrbypasses.io.write_way_mask.foreach(_ := updateWayMask)

  val u = io.update
  XSDebug(io.req.valid,
    p"scTableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
      p"s0_idx=${s0_idx}\n")
  XSDebug(RegNext(io.req.valid),
    p"scTableResp: s1_idx=${s1_idx}," +
      p"ctr:${io.resp.ctrs}\n")
  XSDebug(io.update.mask,
    p"update Table: pc:${Hexadecimal(u.pc)}, " +
      p"tageTakens:${u.tagePreds}, taken:${u.takens}, oldCtr:${u.oldCtrs}\n")
}

class SCThreshold(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val ctr = UInt(ctrBits.W)
  def satPos(ctr: UInt = this.ctr) = ctr === ((1.U << ctrBits) - 1.U)
  def satNeg(ctr: UInt = this.ctr) = ctr === 0.U
  def neutralVal = (1 << (ctrBits - 1)).U
  val thres = UInt(8.W)
  def initVal = 6.U
  def minThres = 6.U
  def maxThres = 31.U
  def update(cause: Bool): SCThreshold = {
    val res = Wire(new SCThreshold(this.ctrBits))
    val newCtr = satUpdate(this.ctr, this.ctrBits, cause)
    val newThres = Mux(res.satPos(newCtr) && this.thres <= maxThres, this.thres + 2.U,
      Mux(res.satNeg(newCtr) && this.thres >= minThres, this.thres - 2.U,
        this.thres))
    res.thres := newThres
    res.ctr := Mux(res.satPos(newCtr) || res.satNeg(newCtr), res.neutralVal, newCtr)
    // XSDebug(true.B, p"scThres Update: cause${cause} newCtr ${newCtr} newThres ${newThres}\n")
    res
  }
}

object SCThreshold {
  def apply(bits: Int)(implicit p: Parameters) = {
    val t = Wire(new SCThreshold(ctrBits=bits))
    t.ctr := t.neutralVal
    t.thres := t.initVal
    t
  }
}

// SCNRows: Int = 512,
// SCNTables: Int = 4,
// SCCtrBits: Int = 6,
// SCHistLens: Seq[Int] = Seq(0, 4, 10, 16),
trait HasSC extends HasSCParameter with HasPerfEvents { this: Tage =>
  val update_on_mispred, update_on_unconf = WireInit(0.U.asTypeOf(Bool()))
  var sc_fh_info = Set[FoldedHistoryInfo]()
  if (EnableSC) {
    val scTables = SCTableInfos.zipWithIndex.map {
      case ((nRows, ctrBits, histLen),idx) => {
        val t = Module(new SCTable(nRows/TageBanks, ctrBits, histLen, parentName = this.parentName + s"scTable${idx}_"))
        val req = t.io.req
        req.valid := io.s0_fire(dupForTageSC)
        req.bits.pc := s0_pc_dup(dupForTageSC)
        req.bits.foldedHist := io.in.bits.foldedHist(dupForTageSC)
        req.bits.ghist := DontCare
        if (!EnableSC) {t.io.update := DontCare}
        t
      }
    }
    sc_fh_info = scTables.map(_.getFoldedHistoryInfo).reduce(_++_).toSet

    val scThresholds = RegInit(SCThreshold(5))
    val useThresholds = scThresholds.thres

    def sign(x: SInt) = x(x.getWidth-1)
    def pos(x: SInt) = !sign(x)
    def neg(x: SInt) = sign(x)

    def aboveThreshold(scSum: SInt, tagePvdr: SInt, threshold: UInt): Bool = {
      val signedThres = threshold.zext
      val totalSum = scSum +& tagePvdr
      (scSum >  signedThres - tagePvdr) && pos(totalSum) ||
        (scSum < -signedThres - tagePvdr) && neg(totalSum)
    }
    val updateThresholds = (useThresholds << 3).asUInt +& 21.U

    val s1_scResps = VecInit(scTables.map(t => t.io.resp))

    val scUpdateMask = WireInit(0.U.asTypeOf(Vec(SCNTables, Bool())))
    val scUpdateTagePreds = Wire(Bool())
    val scUpdateTakens = Wire(Bool())
    val scUpdateOldCtrs = Wire(Vec(SCNTables, SInt(SCCtrBits.W)))
    scUpdateTagePreds := DontCare
    scUpdateTakens := DontCare
    scUpdateOldCtrs := DontCare

    val updateSCMeta = updateMeta.scMeta.get

    val s2_sc_used, s2_conf, s2_unconf, s2_agree, s2_disagree =
      WireInit(0.U.asTypeOf(Bool()))
    val update_sc_used, update_conf, update_unconf, update_agree, update_disagree =
      WireInit(0.U.asTypeOf(Bool()))
    val sc_misp_tage_corr, sc_corr_tage_misp =
      WireInit(0.U.asTypeOf(Bool()))

    // for sc ctrs
    def getCentered(ctr: SInt): SInt = Cat(ctr, 1.U(1.W)).asSInt
    // for tage ctrs, (2*(ctr-4)+1)*8
    def getPvdrCentered(ctr: UInt): SInt = Cat(ctr ^ (1 << (TageCtrBits-1)).U, 1.U(1.W), 0.U(3.W)).asSInt

    val scMeta = tageMeta.scMeta.get
    scMeta := DontCare
    // do summation in s2
    val s1_scTableSums = VecInit(
      (0 to 1) map { i =>
        ParallelSingedExpandingAdd(s1_scResps map (r => getCentered(r.ctrs(i)))) // TODO: rewrite with wallace tree
      }
    )
    val s2_scTableSums = RegEnable(s1_scTableSums, io.s1_fire(dupForTageSC))
    val s2_tagePrvdCtrCentered = getPvdrCentered(RegEnable(s1Resp.ctr, io.s1_fire(dupForTageSC)))
    val s2_totalSums = s2_scTableSums.map(_ +& s2_tagePrvdCtrCentered)
    val s2_sumAboveThresholds = VecInit((0 to 1).map(i => aboveThreshold(s2_scTableSums(i), s2_tagePrvdCtrCentered, useThresholds)))
    val s2_scPreds = VecInit(s2_totalSums.map(_ >= 0.S))

    val s2_scResps = VecInit(RegEnable(s1_scResps, io.s1_fire(dupForTageSC)).map(_.ctrs))
    val s2_scCtrs = VecInit(s2_scResps.map(_(s2PredTaken.asUInt)))
    val s2_chooseBit = s2PredTaken

    val s2_pred =
      Mux(s2Provide && s2_sumAboveThresholds(s2_chooseBit),
        s2_scPreds(s2_chooseBit),
        s2PredTaken
      )

    scMeta.tageTakens := RegEnable(s2PredTaken, io.s2_fire(dupForTageSC))
    scMeta.scUsed     := RegEnable(s2Provide, io.s2_fire(dupForTageSC))
    scMeta.scPreds    := RegEnable(s2_scPreds(s2_chooseBit), io.s2_fire(dupForTageSC))
    scMeta.ctrs       := RegEnable(s2_scCtrs, io.s2_fire(dupForTageSC))

    when (s2Provide) {
      s2_sc_used := true.B
      s2_unconf := !s2_sumAboveThresholds(s2_chooseBit)
      s2_conf := s2_sumAboveThresholds(s2_chooseBit)
      // Use prediction from Statistical Corrector
      XSDebug(p"---------tage_bank provided so that sc used---------\n")
      when (s2_sumAboveThresholds(s2_chooseBit)) {
        val pred = s2_scPreds(s2_chooseBit)
        val debug_pc = Cat(s2DebugPC, 0.U, 0.U(instOffsetBits.W))
        s2_agree := s2PredTaken === pred
        s2_disagree := s2PredTaken =/= pred
        // fit to always-taken condition
        // io.out.s2.fullPred.br_taken(w) := pred
        XSDebug(p"pc(${Hexadecimal(debug_pc)}) SC(${0.U}) overriden pred to ${pred}\n")
      }
    }

    val s3_pred_dup = io.s2_fire.map(f => RegEnable(s2_pred, f))
    val sc_enable_dup = RegNext(dup(io.ctrl.sc_enable))
    for (sc_enable & fp & s3_pred <-
           sc_enable_dup zip io.out.s3.fullPred zip s3_pred_dup) {
      when (sc_enable) {
        fp.br_taken := s3_pred
      }
      dontTouch(sc_enable)
    }

    val updateTageMeta = updateMeta
    when (updateBrJmpValid && updateSCMeta.scUsed) {
      val scPred = updateSCMeta.scPreds
      val tagePred = updateSCMeta.tageTakens
      val taken = updateIn.br_taken
      val scOldCtrs = updateSCMeta.ctrs
      val pvdrCtr = updateTageMeta.providerResps.ctr
      val sum = ParallelSingedExpandingAdd(scOldCtrs.map(getCentered)) +& getPvdrCentered(pvdrCtr)
      val sumAbs = sum.abs.asUInt
      val updateThres = updateThresholds
      val sumAboveThreshold = aboveThreshold(sum, getPvdrCentered(pvdrCtr), updateThres)
      scUpdateTagePreds := tagePred
      scUpdateTakens := taken
      (scUpdateOldCtrs zip scOldCtrs).foreach{case (t, c) => t := c}

      update_sc_used := true.B
      update_unconf := !sumAboveThreshold
      update_conf := sumAboveThreshold
      update_agree := scPred === tagePred
      update_disagree := scPred =/= tagePred
      sc_corr_tage_misp := scPred === taken && tagePred =/= taken && update_conf
      sc_misp_tage_corr := scPred =/= taken && tagePred === taken && update_conf

      val thres = useThresholds
      when (scPred =/= tagePred && sumAbs >= thres - 4.U && sumAbs <= thres - 2.U) {
        val newThres = scThresholds.update(scPred =/= taken)
        scThresholds := newThres
        XSDebug(p"scThres 0 update: old ${useThresholds} --> new ${newThres.thres}\n")
      }

      when (scPred =/= taken || !sumAboveThreshold) {
        scUpdateMask.foreach(_ := true.B)
        XSDebug(sum < 0.S,
          p"scUpdate: bank(${0}), scPred(${scPred}), tagePred(${tagePred}), " +
            p"scSum(-$sumAbs), mispred: sc(${scPred =/= taken}), tage(${updateMispred})\n"
        )
        XSDebug(sum >= 0.S,
          p"scUpdate: bank(${0}), scPred(${scPred}), tagePred(${tagePred}), " +
            p"scSum(+$sumAbs), mispred: sc(${scPred =/= taken}), tage(${updateMispred})\n"
        )
        XSDebug(p"bank(${0}), update: sc: ${updateSCMeta}\n")
        update_on_mispred := scPred =/= taken
        update_on_unconf := scPred === taken
      }
    }


    for (i <- 0 until SCNTables) {
      scTables(i).io.update.mask := RegNext(scUpdateMask(i))
      scTables(i).io.update.tagePreds := RegEnable(scUpdateTagePreds, false.B, updateBrJmpValid)
      scTables(i).io.update.takens    := RegEnable(scUpdateTakens, false.B, updateBrJmpValid)
      scTables(i).io.update.oldCtrs   := RegEnable(scUpdateOldCtrs(i), 0.S, updateBrJmpValid)
      scTables(i).io.update.pc := RegEnable(updateIn.pc, 0.U, updateBrJmpValid)
      scTables(i).io.update.foldedHist := RegEnable(updateGHhis, 0.U.asTypeOf(updateGHhis), updateBrJmpValid)
    }

    tage_perf("sc_conf", PopCount(s2_conf), PopCount(update_conf))
    tage_perf("sc_unconf", PopCount(s2_unconf), PopCount(update_unconf))
    tage_perf("sc_agree", PopCount(s2_agree), PopCount(update_agree))
    tage_perf("sc_disagree", PopCount(s2_disagree), PopCount(update_disagree))
    tage_perf("sc_used", PopCount(s2_sc_used), PopCount(update_sc_used))
    XSPerfAccumulate("sc_update_on_mispred", PopCount(update_on_mispred))
    XSPerfAccumulate("sc_update_on_unconf", PopCount(update_on_unconf))
    XSPerfAccumulate("sc_mispred_but_tage_correct", PopCount(sc_misp_tage_corr))
    XSPerfAccumulate("sc_correct_and_tage_wrong", PopCount(sc_corr_tage_misp))

  }

  override def getFoldedHistoryInfo = Some(tage_fh_info ++ sc_fh_info)

  override val perfEvents = Seq(
    ("tage_tht_hit                  ", PopCount(updateMeta.providers.valid)),
    ("sc_update_on_mispred          ", PopCount(update_on_mispred) ),
    ("sc_update_on_unconf           ", PopCount(update_on_unconf)  ),
  )
  generatePerfEvent()
}