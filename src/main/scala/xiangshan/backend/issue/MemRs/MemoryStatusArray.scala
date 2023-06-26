/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
/***************************************************************************************
  * Author: Liang Sen
  * E-mail: liangsen20z@ict.ac.cn
  * Date: 2023-03-31
  ****************************************************************************************/
package xiangshan.backend.issue.MemRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ChiselAnnotation
import chisel3.util._
import firrtl.passes.InlineAnnotation
import xiangshan.backend.issue.{BasicStatusArrayEntry, EarlyWakeUpInfo, SelectInfo, WakeUpInfo}
import xiangshan.{FuType, Redirect, SrcState, XSBundle, XSModule}
import xs.utils.LogicShiftRight
import xiangshan.backend.issue.MemRs.EntryState._
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.SqPtr

protected[MemRs] object EntryState{
  def s_ready:UInt = 0.U
  def s_wait_cancel: UInt = 1.U
  def s_issued: UInt = 2.U
  def s_wait_replay: UInt = 3.U
  def s_wait_st: UInt = 4.U
  def s_wait_counter: UInt = 5.U
  def apply() = UInt(3.W)
}

sealed class BasicMemoryIssueInfoGenerator(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val in = Input(Valid(new MemoryStatusArrayEntry))
    val out = Output(Valid(new SelectInfo))
  })
  private val iv = io.in.valid
  protected val ib = io.in.bits
  protected val readyToIssue = Wire(Bool())
  io.out.valid := readyToIssue && iv
  io.out.bits.fuType := ib.fuType
  io.out.bits.robPtr := ib.robIdx
  io.out.bits.pdest := ib.pdest
  io.out.bits.fpWen := ib.fpWen
  io.out.bits.rfWen := ib.rfWen
}

class StaLoadIssueInfoGen(implicit p: Parameters) extends BasicMemoryIssueInfoGenerator{
  readyToIssue := ib.srcState(0) === SrcState.rdy && ib.staLoadState === EntryState.s_ready
  io.out.bits.lpv := ib.lpv(0)
}

class StdIssueInfoGen(implicit p: Parameters) extends BasicMemoryIssueInfoGenerator{
  readyToIssue := (ib.srcState(1) === SrcState.rdy || (ib.isCboZero && ib.srcState(0) === SrcState.rdy)) && ib.stdState === EntryState.s_ready
  io.out.bits.fuType := FuType.std
  io.out.bits.lpv := Mux(ib.isCboZero, ib.lpv(0), ib.lpv(1))
}

class MemoryStatusArrayEntry(implicit p: Parameters) extends BasicStatusArrayEntry(2){
  val staLoadState = EntryState()
  val stdState = EntryState()
  val counter = UInt(5.W)
  val isFirstIssue = Bool()
  val waitTarget = new RobPtr
  val sqIdx = new SqPtr
  val isCbo = Bool()
  val isCboZero = Bool()
}

class MemoryStatusArrayEntryUpdateNetwork(stuNum:Int, wakeupWidth:Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val entry = Input(Valid(new MemoryStatusArrayEntry))
    val entryNext = Output(Valid(new MemoryStatusArrayEntry))
    val updateEnable = Output(Bool())
    val enq = Input(Valid(new MemoryStatusArrayEntry))
    val staLduIssue = Input(Bool())
    val stdIssue = Input(Bool())
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val stIssued = Input(Vec(stuNum, Valid(new RobPtr)))
    val replay = Input(Valid(UInt(5.W)))
    val redirect = Input(Valid(new Redirect))
    val stLastCompelet = Input(new SqPtr)
  })

  private val miscNext = WireInit(io.entry)
  private val enqNext = Wire(Valid(new MemoryStatusArrayEntry))
  private val enqUpdateEn = WireInit(false.B)

  //Start of wake up
  private val pregMatch = io.entry.bits.psrc
    .zip(io.entry.bits.srcType)
    .map(p => (io.wakeup ++ io.loadEarlyWakeup).map(elm => (elm.bits.pdest === p._1) && elm.valid && p._2 === elm.bits.destType))
  for ((n, v) <- miscNext.bits.srcState zip pregMatch) {
    val shouldUpdateSrcState = Cat(v).orR
    when(shouldUpdateSrcState) {
      n := SrcState.rdy
    }
  }
  pregMatch.foreach(hv => assert(Mux(io.entry.valid, PopCount(hv) <= 1.U, true.B)))
  private val miscUpdateEnWakeUp = pregMatch.map(_.reduce(_ | _)).reduce(_ | _)
  //End of wake up

  //Start of issue and cancel
  private val imLoad = io.entry.bits.fuType === FuType.ldu
  private val imStore = io.entry.bits.fuType === FuType.stu
  private val needReplay = io.replay.valid
  private val counter = io.entry.bits.counter
  private val counterNext = miscNext.bits.counter
  private val srcShouldBeCancelled = io.entry.bits.lpv.map(l => io.earlyWakeUpCancel.zip(l).map({ case (c, li) => li(0) & c }).reduce(_ | _))
  private val src0HasSpecWakeup = io.entryNext.bits.lpv(0).map(_.orR).reduce(_ || _)
  private val src1HasSpecWakeup = io.entryNext.bits.lpv(1).map(_.orR).reduce(_ || _)
  private val stIssueHit = io.stIssued.map(elm => elm.valid && elm.bits === io.entry.bits.waitTarget).reduce(_ | _)
  private val staLoadIssued = io.staLduIssue
  private val stdIssued = io.stdIssue
  private val staLoadState = io.entry.bits.staLoadState
  private val stdState = io.entry.bits.stdState
  private val staLoadStateNext = miscNext.bits.staLoadState
  private val stdStateNext = miscNext.bits.stdState
  private val miscUpdateEnCancelOrIssue = WireInit(false.B)
  private val shouldBeCanceled = srcShouldBeCancelled.reduce(_ | _)
  private val shouldTurnToReady = staLoadState === s_wait_st && (stIssueHit || io.stLastCompelet >= io.entry.bits.sqIdx)
  miscUpdateEnCancelOrIssue := needReplay || counter.orR || shouldTurnToReady || staLoadIssued || stdIssued || shouldBeCanceled
  private val miscStateUpdateEn = Wire(Bool())
  miscStateUpdateEn := staLoadState === s_wait_cancel || stdState === s_wait_cancel
  switch(staLoadState) {
    is(s_wait_st) {
      when(stIssueHit || io.stLastCompelet >= io.entry.bits.sqIdx) {
        staLoadStateNext := s_ready
      }
    }
    is(s_ready) {
      when(staLoadIssued) {
        staLoadStateNext := Mux(src0HasSpecWakeup, s_wait_cancel, s_wait_replay)
      }
    }
    is(s_wait_cancel) {
      staLoadStateNext := Mux(srcShouldBeCancelled(0), s_ready, s_wait_replay)
    }
    is(s_wait_replay) {
      when(needReplay) {
        staLoadStateNext := Mux(io.replay.bits === 0.U, s_ready, s_wait_counter)
      }.elsewhen(counter(0).asBool) {
        staLoadStateNext := s_issued
      }
    }
    is(s_wait_counter) {
      when(counter(0).asBool) {
        staLoadStateNext := s_ready
      }
    }
  }

  when(imStore) {
    //STD State machine
    switch(stdState) {
      is(s_ready) {
        when(stdIssued) {
          stdStateNext := Mux(src1HasSpecWakeup, s_wait_cancel, s_issued)
        }
      }
      is(s_wait_cancel) {
        stdStateNext := Mux(srcShouldBeCancelled(1), s_ready, s_issued)
      }
    }
  }

  when(io.replay.valid){
    counterNext := io.replay.bits
  }.elsewhen(staLoadStateNext =/= s_ready && staLoadState === s_ready) {
    counterNext := (1 << 4).U
  }.elsewhen(counter.orR) {
    counterNext := LogicShiftRight(counter, 1)
  }
  assert(Mux(io.entry.valid, Cat(srcShouldBeCancelled(0), staLoadIssued) <= 2.U, true.B))
  assert(Mux(io.entry.valid && !io.entry.bits.isCboZero, Cat(srcShouldBeCancelled(1), stdIssued) <= 2.U, true.B))
  assert(Mux(io.entry.valid && io.entry.bits.isCboZero, Cat(srcShouldBeCancelled(0), stdIssued) <= 2.U, true.B))
  assert(Mux(staLoadIssued, io.entry.valid && staLoadState === s_ready, true.B))
  assert(Mux(stdIssued, io.entry.valid && stdState === s_ready && imStore, true.B))
  assert(Mux(io.entry.valid && imLoad, stdState === s_issued, true.B))
  assert(Mux(io.entry.valid,
    staLoadState === s_wait_st ||
      staLoadState === s_ready ||
      staLoadState === s_wait_cancel ||
      staLoadState === s_wait_counter ||
      staLoadState === s_wait_replay ||
      staLoadState === s_issued, true.B))
  assert(Mux(io.entry.valid && imStore, stdState === s_ready || stdState === s_wait_cancel || stdState === s_issued, true.B))

  srcShouldBeCancelled.zip(miscNext.bits.srcState).foreach{case(en, state) => when(en){state := SrcState.busy}}
  //End of issue and cancel

  //Start of dequeue and redirect
  private val shouldBeFlushed = io.entry.valid & io.entry.bits.robIdx.needFlush(io.redirect)
  private val miscUpdateEnDequeueOrRedirect =
    (staLoadStateNext === EntryState.s_issued && stdStateNext === EntryState.s_issued) || shouldBeFlushed
  when(miscUpdateEnDequeueOrRedirect) {
    miscNext.valid := false.B
  }
  //End of dequeue and redirect

  //Start of LPV behaviors
  private val lpvModified = Wire(Vec(miscNext.bits.lpv.length, Vec(miscNext.bits.lpv.head.length, Bool())))
  lpvModified.foreach(_.foreach(_ := false.B))
  for(((((newLpvs, oldLpvs),mod), st), psrc) <- miscNext.bits.lpv.zip(io.entry.bits.lpv).zip(lpvModified).zip(io.entry.bits.srcType).zip(io.entry.bits.psrc)){
    for(((((nl, ol), ewkp), m),idx) <- newLpvs.zip(oldLpvs).zip(io.loadEarlyWakeup).zip(mod).zipWithIndex){
      val earlyWakeUpHit = ewkp.valid && ewkp.bits.pdest === psrc && st === ewkp.bits.destType
      val regularWakeupHits = io.wakeup.map(wkp => wkp.valid && wkp.bits.pdest === psrc && st === wkp.bits.destType)
      val regularWakeupLpv = io.wakeup.map(wkp => wkp.bits.lpv(idx))
      val lpvUpdateHitsVec = regularWakeupHits :+ earlyWakeUpHit
      val lpvUpdateDataVec = regularWakeupLpv :+ ewkp.bits.lpv
      val wakeupLpvValid = lpvUpdateHitsVec.reduce(_|_)
      val wakeupLpvSelected = Mux1H(lpvUpdateHitsVec, lpvUpdateDataVec)
      nl := Mux(wakeupLpvValid, wakeupLpvSelected, LogicShiftRight(ol,1))
      m := wakeupLpvValid | ol.orR
      assert(Mux(io.entry.valid, PopCount(lpvUpdateHitsVec) === 1.U || PopCount(lpvUpdateHitsVec) === 0.U, true.B))
    }
  }
  private val miscUpdateEnLpvUpdate = lpvModified.map(_.reduce(_|_)).reduce(_|_)
  //End of LPV behaviors

  //Start of Enqueue
  enqNext.bits := io.enq.bits
  private val enqShouldBeSuppressed = io.enq.bits.robIdx.needFlush(io.redirect)
  enqNext.valid := !enqShouldBeSuppressed && io.enq.valid
  enqUpdateEn := enqNext.valid
  //End of Enqueue

  io.updateEnable := Mux(io.entry.valid, miscUpdateEnWakeUp | miscUpdateEnCancelOrIssue | miscUpdateEnDequeueOrRedirect | miscUpdateEnLpvUpdate | miscStateUpdateEn, enqUpdateEn)
  io.entryNext := Mux(enqUpdateEn, enqNext, miscNext)
}
class Replay(entryNum:Int) extends Bundle {
  val entryIdxOH = UInt(entryNum.W)
  val waitVal = UInt(5.W)
}

class MemoryStatusArray(entryNum:Int, stuNum:Int, lduNum:Int, wakeupWidth:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))

    val staSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val stdSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val lduSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))

    val allocateInfo = Output(UInt(entryNum.W))

    val enq = Input(Valid(new Bundle{
      val addrOH = UInt(entryNum.W)
      val data = new MemoryStatusArrayEntry
    }))

    val staLduIssue = Input(Valid(UInt(entryNum.W)))
    val stdIssue = Input(Valid(UInt(entryNum.W)))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val replay = Input(Vec(2, Valid(new Replay(entryNum))))
    val stIssued = Input(Vec(stuNum, Valid(new RobPtr)))
    val stLastCompelet = Input(new SqPtr)
  })

  private val statusArray = Reg(Vec(entryNum, new MemoryStatusArrayEntry))
  private val statusArrayValid = RegInit(VecInit(Seq.fill(entryNum)(false.B)))
  private val statusArrayValidAux = RegInit(VecInit(Seq.fill(entryNum)(false.B)))

  //Start of select logic
  private val selInfoOut = Seq(io.staSelectInfo, io.stdSelectInfo, io.lduSelectInfo)
  private val infoGenSeq = Seq(
    Seq.fill(entryNum)(Module(new StaLoadIssueInfoGen)),
    Seq.fill(entryNum)(Module(new StdIssueInfoGen)),
    Seq.fill(entryNum)(Module(new StaLoadIssueInfoGen))
  )
  for((so, gens) <- selInfoOut.zip(infoGenSeq)){
    for ((((selInfo, saEntry), saValid), cvt) <- so
      .zip(statusArray)
      .zip(statusArrayValid)
      .zip(gens)){
      cvt.io.in.valid := saValid
      cvt.io.in.bits := saEntry
      selInfo := cvt.io.out
    }
  }
  //End of select logic

  //Start of allocate logic
  io.allocateInfo := Cat(statusArrayValidAux.reverse)
  //End of allocate logic
  for((((v, va), d), idx) <- statusArrayValid
    .zip(statusArrayValidAux)
    .zip(statusArray)
    .zipWithIndex
      ){
    val updateNetwork = Module(new MemoryStatusArrayEntryUpdateNetwork(stuNum,wakeupWidth))
    updateNetwork.io.entry.valid := v
    updateNetwork.io.entry.bits := d
    updateNetwork.io.enq.valid := io.enq.valid & io.enq.bits.addrOH(idx)
    updateNetwork.io.enq.bits := io.enq.bits.data
    updateNetwork.io.staLduIssue := io.staLduIssue.valid && io.staLduIssue.bits(idx)
    updateNetwork.io.stdIssue := io.stdIssue.valid && io.stdIssue.bits(idx)
    updateNetwork.io.wakeup := io.wakeup
    updateNetwork.io.loadEarlyWakeup := io.loadEarlyWakeup
    updateNetwork.io.earlyWakeUpCancel := io.earlyWakeUpCancel

    val replaySels = io.replay.map(r => r.valid && r.bits.entryIdxOH(idx))
    val replayVals = io.replay.map(_.bits.waitVal)
    updateNetwork.io.replay.valid := replaySels.reduce(_|_)
    updateNetwork.io.replay.bits := Mux1H(replaySels, replayVals)
    updateNetwork.io.redirect := io.redirect
    assert(Mux(v, PopCount(replaySels) === 1.U || PopCount(replaySels) === 0.U, true.B))
    updateNetwork.io.stIssued := io.stIssued
    updateNetwork.io.stLastCompelet := io.stLastCompelet

    val en = updateNetwork.io.updateEnable
    when(en) {
      v  := updateNetwork.io.entryNext.valid
      va := updateNetwork.io.entryNext.valid
      d  := updateNetwork.io.entryNext.bits
    }
  }

  assert(Cat(statusArrayValid) === Cat(statusArrayValidAux))
  assert(Mux(io.enq.valid, PopCount(io.enq.bits.addrOH) === 1.U, true.B))
  assert((Mux(io.enq.valid, io.enq.bits.addrOH, 0.U) & Cat(statusArrayValid.reverse)) === 0.U)
  private val issues = Seq(io.staLduIssue, io.stdIssue)
  for(iss <- issues){
    assert(Mux(iss.valid, PopCount(iss.bits & Cat(statusArrayValid.reverse)) === 1.U, true.B))
  }
}
