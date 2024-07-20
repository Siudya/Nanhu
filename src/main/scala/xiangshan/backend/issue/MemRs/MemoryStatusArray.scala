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
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.{ChiselAnnotation, prefix}
import chisel3.util._
import firrtl.passes.InlineAnnotation
import xiangshan.backend.issue.{BasicStatusArrayEntry, EarlyWakeUpInfo, SelectInfo, WakeUpInfo}
import xiangshan.{FuType, Redirect, SrcState, SrcType, XSBundle, XSModule}
import xs.utils.{LogicShiftLeft, LogicShiftRight, PickOneLow}
import xiangshan.backend.issue.MemRs.EntryState._
import xiangshan.backend.issue.MemRs.UpdateNetworkHelper.{EarlyWkpToNormalWkp, WakeupLogics}
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
  io.out.bits.isVector := ib.isVector
  io.out.bits.psrc := ib.psrc
  io.out.bits.vm := ib.vm
  io.out.bits.isFma := false.B
  io.out.bits.isSgOrStride := ib.srcType(1) === SrcType.reg || ib.srcType(1) === SrcType.vec
  io.out.bits.ftqPtr := ib.ftqPtr
  io.out.bits.ftqOffset := ib.ftqOffset
}

class StaLoadIssueInfoGen(implicit p: Parameters) extends BasicMemoryIssueInfoGenerator{
  private val issCond = ib.srcState(0) === SrcState.rdy && ib.srcState(1) === SrcState.rdy &&
    Mux(FuType.isStore(ib.fuType),
      true.B,
      ib.srcState(2) === SrcState.rdy
    ) &&
    ib.vmState === SrcState.rdy
  readyToIssue := issCond && ib.staLoadState === EntryState.s_ready
  io.out.bits.lpv := VecInit(ib.lpv(0).zip(ib.lpv(1)).map({case(a, b) => a | b}))
}

class StdIssueInfoGen(implicit p: Parameters) extends BasicMemoryIssueInfoGenerator{
  readyToIssue := (ib.srcState(2) === SrcState.rdy || (ib.isCboZero && ib.srcState(0) === SrcState.rdy)) && ib.stdState === EntryState.s_ready
  io.out.bits.fuType := FuType.std
  io.out.bits.lpv := Mux(ib.isCboZero, ib.lpv(0), ib.lpv(2))
}

class MemoryStatusArrayEntry(implicit p: Parameters) extends BasicStatusArrayEntry(3) {
  val vm = UInt(PhyRegIdxWidth.W)
  val vmState = SrcState()
  val isVector = Bool()
  val staLoadState = EntryState()
  val stdState = EntryState()
  val counter = UInt(6.W)
  val replayPenalty = UInt(4.W)
  val isFirstIssue = Bool()
  val waitTarget = new RobPtr
  val sqIdx = new SqPtr
  val isCbo = Bool()
  val isCboZero = Bool()
}

object UpdateNetworkHelper{
  //Return (updateValid, newSrcState, newLpvs)
  def WakeupLogics(valid: Bool, psrc: UInt, srcType: UInt, srcState: UInt, lpvs: Vec[UInt], wkps: Seq[Valid[WakeUpInfo]], maybeReg: Boolean, idx: Int): (Bool, UInt, Vec[UInt]) = {
    val updateValid = Wire(Bool())
    val newState = WireInit(srcState)
    val newLpvs = WireInit(lpvs)
    prefix(s"wkp_${idx}") {
      val wakeUpValid = Wire(Bool())
      val hitVec = Wire(Vec(wkps.length, Bool()))
      hitVec.zip(wkps).foreach({ case (hv, wkp) =>
        if (maybeReg) {
          hv := wkp.valid && wkp.bits.pdest === psrc && wkp.bits.destType === srcType && !(srcType === SrcType.reg && psrc === 0.U)
        } else {
          hv := wkp.valid && wkp.bits.pdest === psrc && wkp.bits.destType === srcType
        }
      })
      val lpvsSel = Mux1H(hitVec, wkps.map(_.bits.lpv))
      wakeUpValid := hitVec.reduce(_ | _)
      when(wakeUpValid) {
        newState := SrcState.rdy
      }
      newLpvs.zip(lpvs).zip(lpvsSel).foreach({ case ((nl, ol), wl) =>
        when(wakeUpValid) {
          nl := wl
        }.elsewhen(ol.orR) {
          nl := LogicShiftRight(ol, 1)
        }
        when(valid) {assert(PopCount(nl) <= 1.U, s"psrc${idx}: LPV should only be ont-bit valid")}
      })
      updateValid := wakeUpValid || lpvs.map(_.orR).reduce(_ | _)
      when(valid) {assert(PopCount(hitVec) <= 1.U, s"psrc${idx}: Multiple wake-ups hit!")}
    }
    (updateValid, newState, newLpvs)
  }

  def EarlyWkpToNormalWkp(in:Vec[Valid[EarlyWakeUpInfo]], p:Parameters):Vec[Valid[WakeUpInfo]] = {
    val loadUnitNum = in.length
    val res = Wire(Vec(loadUnitNum, Valid(new WakeUpInfo()(p))))
    for(((n, e), i) <- res.zip(in).zipWithIndex){
      n.valid := e.valid
      n.bits.pdest := e.bits.pdest
      n.bits.destType := e.bits.destType
      n.bits.robPtr := e.bits.robPtr
      n.bits.lpv.foreach(_ := 0.U)
      n.bits.lpv(i) := e.bits.lpv
    }
    res
  }
}

class MemoryStatusArrayEntryUpdateNetwork(stuNum:Int, wakeupWidth:Int, regWkpIdx:Seq[Int], fpWkpIdx:Seq[Int], vecWkpIdx:Seq[Int])(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val entry = Input(Valid(new MemoryStatusArrayEntry))
    val entryNext = Output(Valid(new MemoryStatusArrayEntry))
    val updateEnable = Output(Bool())
    val enq = Input(Valid(new MemoryStatusArrayEntry))
    val staLduIssue = Input(Bool())
    val stdIssue = Input(Bool())
    val wakeups = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val stIssued = Input(Vec(stuNum, Valid(new RobPtr)))
    val replay = Input(Valid(UInt(3.W)))
    val redirect = Input(Valid(new Redirect))
    val stLastCompelet = Input(new SqPtr)
  })
  require((regWkpIdx ++ fpWkpIdx ++ vecWkpIdx).distinct.length == wakeupWidth)
  require((regWkpIdx ++ fpWkpIdx ++ vecWkpIdx).distinct.max == wakeupWidth - 1)
  private val miscNext = WireInit(io.entry)
  private val enqNext = Wire(Valid(new MemoryStatusArrayEntry))
  private val enqUpdateEn = WireInit(false.B)

  //Start of wake up && LPV update
  private val earlyWakeUps = EarlyWkpToNormalWkp(io.loadEarlyWakeup, p)
  private val src0WakeUps = regWkpIdx.map(io.wakeups(_)) ++ earlyWakeUps
  private val src1WakeUps = (regWkpIdx ++ vecWkpIdx).distinct.map(io.wakeups(_)) ++ earlyWakeUps
  private val src2WakpUps = (regWkpIdx ++ fpWkpIdx ++ vecWkpIdx).distinct.map(io.wakeups(_)) ++ earlyWakeUps
  private val vmWakpUps = vecWkpIdx.map(io.wakeups(_))

  private val (src0UpdateValid, src0NewState, src0NewLpvs) = WakeupLogics(
    io.entry.valid, io.entry.bits.psrc(0),
    io.entry.bits.srcType(0), io.entry.bits.srcState(0),
    io.entry.bits.lpv(0), src0WakeUps, true, 0
  )
  private val (src1UpdateValid, src1NewState, src1NewLpvs) = WakeupLogics(
    io.entry.valid, io.entry.bits.psrc(1),
    io.entry.bits.srcType(1), io.entry.bits.srcState(1),
    io.entry.bits.lpv(1), src1WakeUps, true, 1
  )
  private val (src2UpdateValid, src2NewState, src2NewLpvs) = WakeupLogics(
    io.entry.valid, io.entry.bits.psrc(2),
    io.entry.bits.srcType(2), io.entry.bits.srcState(2),
    io.entry.bits.lpv(2), src2WakpUps, true, 2
  )
  private val (vmUpdateValid, vmNewState, _) = WakeupLogics(
    io.entry.valid, io.entry.bits.vm,
    SrcType.vec, io.entry.bits.vmState,
    VecInit(Seq.fill(loadUnitNum)(0.U(LpvLength.W))), vmWakpUps, false, 3
  )
  when(src0UpdateValid){
    miscNext.bits.srcState(0) := src0NewState
    miscNext.bits.lpv(0) := src0NewLpvs
  }
  when(src1UpdateValid) {
    miscNext.bits.srcState(1) := src1NewState
    miscNext.bits.lpv(1) := src1NewLpvs
  }
  when(src2UpdateValid) {
    miscNext.bits.srcState(2) := src2NewState
    miscNext.bits.lpv(2) := src2NewLpvs
  }
  when(vmUpdateValid) {
    miscNext.bits.vmState := vmNewState
  }
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
  private val src2HasSpecWakeup = io.entryNext.bits.lpv(2).map(_.orR).reduce(_ || _)
  private val stIssueHit = io.stIssued.map(elm => elm.valid && elm.bits === io.entry.bits.waitTarget).reduce(_ | _)
  private val staLoadIssued = io.staLduIssue
  private val stdIssued = io.stdIssue
  private val staLoadState = io.entry.bits.staLoadState
  private val stdState = io.entry.bits.stdState
  private val staLoadStateNext = miscNext.bits.staLoadState
  private val stdStateNext = miscNext.bits.stdState
  private val isSgOrStrd = io.entry.bits.isVector && (io.entry.bits.srcType(1) === SrcType.reg || io.entry.bits.srcType(1) === SrcType.vec)
  private val shouldTurnToReady = staLoadState === s_wait_st && (stIssueHit || io.stLastCompelet >= io.entry.bits.sqIdx)
  private val addrShouldBeCancelled = srcShouldBeCancelled(0) | srcShouldBeCancelled(1)
  switch(staLoadState) {
    is(s_wait_st) {
      when(stIssueHit || io.stLastCompelet >= io.entry.bits.sqIdx) {
        staLoadStateNext := s_ready
      }
    }
    is(s_ready) {
      when(staLoadIssued) {
        staLoadStateNext := Mux(src0HasSpecWakeup || src1HasSpecWakeup, s_wait_cancel, s_wait_replay)
      }
    }
    is(s_wait_cancel) {
      staLoadStateNext := Mux(addrShouldBeCancelled, s_ready, s_wait_replay)
    }
    is(s_wait_replay) {
      when(needReplay) {
        staLoadStateNext := Mux(io.replay.bits === 0.U && io.entry.bits.replayPenalty === 0.U, s_ready, s_wait_counter)
      }.elsewhen(counter === 1.U) {
        staLoadStateNext := s_issued
      }
    }
    is(s_wait_counter) {
      when(counter === 1.U) {
        staLoadStateNext := s_ready
      }
    }
  }

  when(imStore) {
    //STD State machine
    switch(stdState) {
      is(s_ready) {
        when(stdIssued) {
          stdStateNext := Mux(src2HasSpecWakeup, s_wait_cancel, s_issued)
        }
      }
      is(s_wait_cancel) {
        stdStateNext := Mux(srcShouldBeCancelled(2), s_ready, s_issued)
      }
    }
  }

  when(io.replay.valid){
    counterNext := io.replay.bits +& Cat(io.entry.bits.replayPenalty, 0.U(1.W))
  }.elsewhen(staLoadStateNext =/= s_ready && staLoadState === s_ready) {
    counterNext := Mux(isSgOrStrd, 6.U, 5.U)
  }.elsewhen(counter.orR) {
    counterNext := counter - 1.U
  }

  when(io.replay.valid){
    miscNext.bits.replayPenalty := Mux(io.entry.bits.replayPenalty === 0xF.U, 0xF.U, io.entry.bits.replayPenalty + 1.U)
  }

  when(io.entry.valid){
    assert(Cat(addrShouldBeCancelled, staLoadIssued) <= 2.U)
    assert(staLoadState === s_wait_st || staLoadState === s_ready ||
      staLoadState === s_wait_cancel || staLoadState === s_wait_counter ||
      staLoadState === s_wait_replay || staLoadState === s_issued)
  }
  when(io.entry.valid && !io.entry.bits.isCboZero){assert(Cat(srcShouldBeCancelled(2), stdIssued) <= 2.U)}
  when(io.entry.valid && io.entry.bits.isCboZero){assert(Cat(srcShouldBeCancelled(0), stdIssued) <= 2.U)}
  when(staLoadIssued){assert(io.entry.valid && staLoadState === s_ready)}
  when(stdIssued){assert(io.entry.valid && stdState === s_ready && imStore)}
  when(io.entry.valid && imLoad){assert(stdState === s_issued)}
  when(io.entry.valid && imStore){assert(stdState === s_ready || stdState === s_wait_cancel || stdState === s_issued)}

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

  //Start of Enqueue
  enqNext.bits := io.enq.bits
  enqNext.valid := io.enq.valid
  enqUpdateEn := enqNext.valid
  //End of Enqueue

  io.updateEnable := io.entry.valid | enqUpdateEn
  io.entryNext := Mux(enqUpdateEn, enqNext, miscNext)
}
class Replay(entryNum:Int) extends Bundle {
  val entryIdxOH = UInt(entryNum.W)
  val waitVal = UInt(3.W)
}

class MemoryStatusArray(entryNum:Int, stuNum:Int, wakeupWidth:Int, regWkpIdx:Seq[Int], fpWkpIdx:Seq[Int], vecWkpIdx:Seq[Int])(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))

    val staSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val stdSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val lduSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))

    val alloc = Output(Bool())

    val enq = Input(Valid(new MemoryStatusArrayEntry))
    val enqAddr = Output(UInt(entryNum.W))

    val staLduIssue = Input(Valid(UInt(entryNum.W)))
    val stdIssue = Input(Valid(UInt(entryNum.W)))
    val wakeups = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val replay = Input(Vec(3, Valid(new Replay(entryNum))))
    val stIssued = Input(Vec(stuNum, Valid(new RobPtr)))
    val stLastCompelet = Input(new SqPtr)
  })

  private val statusArray = Reg(Vec(entryNum, new MemoryStatusArrayEntry))
  private val statusArrayValid = RegInit(VecInit(Seq.fill(entryNum)(false.B)))

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
  private val validNextWire = WireInit(statusArrayValid)
  private val allocAddr = PickOneLow(validNextWire)
  private val allocReg = RegNext(allocAddr.valid, false.B)
  private val allocAddrReg = RegEnable(allocAddr.bits, allocAddr.valid)
  io.alloc := allocReg
  io.enqAddr := allocAddrReg
  //End of allocate logic
  for((((v, va), d), idx) <- statusArrayValid
    .zip(validNextWire)
    .zip(statusArray)
    .zipWithIndex
      ){
    val updateNetwork = Module(new MemoryStatusArrayEntryUpdateNetwork(stuNum, wakeupWidth, regWkpIdx, fpWkpIdx, vecWkpIdx))
    updateNetwork.io.entry.valid := v
    updateNetwork.io.entry.bits := d
    updateNetwork.io.enq.valid := io.enq.valid & allocAddrReg(idx)
    updateNetwork.io.enq.bits := io.enq.bits
    updateNetwork.io.staLduIssue := io.staLduIssue.valid && io.staLduIssue.bits(idx)
    updateNetwork.io.stdIssue := io.stdIssue.valid && io.stdIssue.bits(idx)
    updateNetwork.io.wakeups := io.wakeups
    updateNetwork.io.loadEarlyWakeup := io.loadEarlyWakeup
    updateNetwork.io.earlyWakeUpCancel := io.earlyWakeUpCancel

    val replaySels = io.replay.map(r => r.valid && r.bits.entryIdxOH(idx))
    val replayVals = io.replay.map(_.bits.waitVal)
    updateNetwork.io.replay.valid := replaySels.reduce(_|_)
    updateNetwork.io.replay.bits := Mux1H(replaySels, replayVals)
    updateNetwork.io.redirect := io.redirect
    when(v){assert(PopCount(replaySels) <= 1.U)}
    updateNetwork.io.stIssued := io.stIssued
    updateNetwork.io.stLastCompelet := io.stLastCompelet

    val en = updateNetwork.io.updateEnable
    when(en) {
      v  := updateNetwork.io.entryNext.valid
      va := updateNetwork.io.entryNext.valid
      d  := updateNetwork.io.entryNext.bits
    }
  }

  when(io.enq.valid){assert(PopCount(allocAddrReg) === 1.U)}
  assert((Mux(io.enq.valid, allocAddrReg, 0.U) & Cat(statusArrayValid.reverse)) === 0.U)
  private val issues = Seq(io.staLduIssue, io.stdIssue)
  for(iss <- issues){
    when(iss.valid){assert((PopCount(iss.bits & Cat(statusArrayValid.reverse)) <= 2.U) && (iss.bits & Cat(statusArrayValid.reverse)).orR)}
  }
}
