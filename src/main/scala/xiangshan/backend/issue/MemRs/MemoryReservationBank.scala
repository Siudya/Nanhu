/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
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
 * Date: 2023-06-19
 ****************************************************************************************/
package xiangshan.backend.issue.MemRs

import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.issue._
import chisel3._
import chisel3.util._
import xiangshan.{FuType, LSUOpType, MicroOp, Redirect, SrcState, SrcType, XSCoreParamsKey}
import xiangshan.backend.issue.MemRs.EntryState._
import xiangshan.backend.issue.{EarlyWakeUpInfo, WakeUpInfo}
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.SqPtr

class MemoryReservationBank(entryNum:Int, stuNum:Int, wakeupWidth:Int, regWkpIdx:Seq[Int], fpWkpIdx:Seq[Int], vecWkpIdx:Seq[Int])(implicit p: Parameters) extends Module{
  private val loadUnitNum = p(XSCoreParamsKey).exuParameters.LduCnt
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))

    val staSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val stdSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val lduSelectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val allocateInfo = Output(UInt(entryNum.W))

    val enq = Input(Valid(new Bundle {
      val addrOH = UInt(entryNum.W)
      val data = new MicroOp
    }))

    val loadIssue = Input(Valid(UInt(entryNum.W)))
    val loadUop = Output(new MicroOp)
    val staIssue = Input(Valid(UInt(entryNum.W)))
    val staUop = Output(new MicroOp)
    val stdIssue = Input(Valid(UInt(entryNum.W)))
    val stdUop = Output(new MicroOp)
    val specialIssue = Input(Valid(UInt(entryNum.W)))
    val auxLoadIssValid = Input(Bool())
    val auxStaIssValid = Input(Bool())
    val auxStdIssValid = Input(Bool())

    val replay = Input(Vec(3, Valid(new Replay(entryNum))))

    val stIssued = Input(Vec(stuNum, Valid(new RobPtr)))
    val stLastCompelet = Input(new SqPtr)

    val wakeups = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })


  private val statusArray = Module(new MemoryStatusArray(entryNum, stuNum, wakeupWidth, regWkpIdx, fpWkpIdx, vecWkpIdx))
  private val payloadArray = Module(new PayloadArray(new MicroOp, entryNum, 3, "MemoryPayloadArray"))

  private def EnqToEntry(in: MicroOp): MemoryStatusArrayEntry = {
    val stIssueHit = io.stIssued.map(st => st.valid && st.bits === in.cf.waitForRobIdx).reduce(_|_)
    val shouldWait = in.ctrl.fuType === FuType.ldu && in.cf.loadWaitBit && in.sqIdx > io.stLastCompelet && !stIssueHit
    val isCbo = LSUOpType.isCbo(in.ctrl.fuOpType)
    val isCboZero = in.ctrl.fuOpType === LSUOpType.cbo_zero
    val enqEntry = Wire(new MemoryStatusArrayEntry)
    val isVector = in.ctrl.isVector
    // psrc0: base
    // psrc1: stride/offset
    // psrc2: data/old_vd
    enqEntry.psrc(0) := in.psrc(0)
    enqEntry.srcType(0) := in.ctrl.srcType(0)
    enqEntry.srcState(0) := Mux(SrcType.isReg(in.ctrl.srcType(0)), in.srcState(0), SrcState.rdy)
    enqEntry.lpv.foreach(_.foreach(_ := 0.U))
    enqEntry.fuType := in.ctrl.fuType
    enqEntry.rfWen := in.ctrl.rfWen
    enqEntry.fpWen := in.ctrl.fpWen
    enqEntry.robIdx := in.robIdx
    enqEntry.sqIdx := in.sqIdx
    enqEntry.pdest := in.pdest
    enqEntry.waitTarget := in.cf.waitForRobIdx
    enqEntry.isFirstIssue := false.B
    enqEntry.counter := 0.U
    enqEntry.isCbo := isCbo
    enqEntry.isCboZero := isCboZero
    enqEntry.isVector := isVector
    enqEntry.replayPenalty := 0.U
    when(!isVector){
      enqEntry.psrc(1) := DontCare
      enqEntry.psrc(2) := in.psrc(1)
      enqEntry.vm := DontCare
      enqEntry.srcType(1) := SrcType.default
      enqEntry.srcType(2) := in.ctrl.srcType(1)
      enqEntry.vmState := SrcState.rdy
      enqEntry.srcState(1) := SrcState.rdy
      enqEntry.srcState(2) := Mux(SrcType.isRegOrFp(in.ctrl.srcType(1)), in.srcState(1), SrcState.rdy)
      //STAState handles LOAD, STORE, CBO.INVAL, CBO.FLUSH, CBO.CLEAN, PREFECTH.R, PREFETCH.W
      enqEntry.staLoadState := Mux(in.ctrl.fuType === FuType.stu && isCboZero, s_issued, Mux(shouldWait, s_wait_st, s_ready))
      //STDState handles STORE,CBO.ZERO
      enqEntry.stdState := Mux(in.ctrl.fuType === FuType.stu && !isCbo, s_ready, s_issued)
    }.otherwise{
      val tailAgnostic = (in.vCsrInfo.vta(0) && in.isTail)
      val inactiveAgnostic = (in.vCsrInfo.vma(0) && in.ctrl.vm)
      val src2NeedWakeup = !(tailAgnostic || inactiveAgnostic)
      enqEntry.psrc(1) := in.psrc(1)
      enqEntry.psrc(2) := in.psrc(2)
      enqEntry.vm := in.vm
      enqEntry.srcType(1) := in.ctrl.srcType(1)
      enqEntry.srcType(2) := Mux(src2NeedWakeup, SrcType.vec, SrcType.default)
      enqEntry.srcState(1) := Mux(SrcType.needWakeup(in.ctrl.srcType(1)), in.srcState(1), SrcState.rdy)
      enqEntry.srcState(2) := Mux(src2NeedWakeup, in.srcState(2), SrcState.rdy)
      enqEntry.vmState := Mux(in.ctrl.vm && in.vCsrInfo.vma(0), in.vmState, SrcState.rdy)
      enqEntry.staLoadState := s_ready
      enqEntry.stdState := Mux(FuType.isStore(in.ctrl.fuType), s_ready, s_issued)
    }
    enqEntry
  }

  statusArray.io.redirect := io.redirect
  io.staSelectInfo := statusArray.io.staSelectInfo
  io.stdSelectInfo := statusArray.io.stdSelectInfo
  io.lduSelectInfo := statusArray.io.lduSelectInfo
  io.allocateInfo := statusArray.io.allocateInfo
  statusArray.io.enq.valid := io.enq.valid
  statusArray.io.enq.bits.addrOH := io.enq.bits.addrOH
  statusArray.io.enq.bits.data := EnqToEntry(io.enq.bits.data)
  statusArray.io.staLduIssue.valid := io.loadIssue.valid || io.staIssue.valid || io.specialIssue.valid
  statusArray.io.stdIssue.valid := io.stdIssue.valid
  statusArray.io.staLduIssue.bits := Seq(io.loadIssue, io.staIssue, io.specialIssue).map(e => Mux(e.valid, e.bits, 0.U)).reduce(_|_)
  statusArray.io.stdIssue.bits := io.stdIssue.bits
  statusArray.io.replay := io.replay
  statusArray.io.stIssued.zip(io.stIssued).foreach({case(a, b) => a := Pipe(b)})
  statusArray.io.wakeups := io.wakeups
  statusArray.io.loadEarlyWakeup := io.loadEarlyWakeup
  statusArray.io.earlyWakeUpCancel := io.earlyWakeUpCancel
  statusArray.io.stLastCompelet := io.stLastCompelet

  payloadArray.io.write.en := io.enq.valid
  payloadArray.io.write.addr := io.enq.bits.addrOH
  payloadArray.io.write.data := io.enq.bits.data

  payloadArray.io.read(0).addr := RegEnable(io.loadIssue.bits, io.auxLoadIssValid)
  payloadArray.io.read(1).addr := RegEnable(io.staIssue.bits, io.auxStaIssValid)
  payloadArray.io.read(2).addr := RegEnable(io.stdIssue.bits, io.auxStdIssValid)
  io.loadUop := payloadArray.io.read(0).data
  io.staUop := payloadArray.io.read(1).data
  io.stdUop := payloadArray.io.read(2).data

  when(io.loadIssue.valid){assert(PopCount(io.loadIssue.bits) === 1.U)}
  when(io.staIssue.valid){assert(PopCount(io.staIssue.bits) === 1.U)}
  when(io.stdIssue.valid){assert(PopCount(io.stdIssue.bits) === 1.U)}
}

