package xiangshan.backend.issue.MemRs

import chipsalliance.rocketchip.config.Parameters
import xiangshan.backend.issue._
import chisel3._
import chisel3.util._
import xiangshan.{FuType, MicroOp, Redirect}
import xiangshan.backend.issue.MemRs.EntryState._
import xiangshan.backend.issue.{EarlyWakeUpInfo, WakeUpInfo}
import xiangshan.backend.rob.RobPtr

class MemoryReservationBank(entryNum:Int, stuNum:Int, lduNum:Int, wakeupWidth:Int)(implicit p: Parameters) extends Module{
  private val issueWidth = 3
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

    val staIssue = Input(Valid(UInt(entryNum.W)))
    val stdIssue = Input(Valid(UInt(entryNum.W)))
    val lduIssue = Input(Valid(UInt(entryNum.W)))
    val staIssueUop = Output(new MicroOp)
    val stdIssueUop = Output(new MicroOp)
    val lduIssueUop = Output(new MicroOp)

    val replay = Input(Vec(2, Valid(new Bundle {
      val entryIdxOH = UInt(entryNum.W)
      val waitVal = UInt(5.W)
    })))
    val stIssued = Input(Vec(stuNum, Valid(new RobPtr)))
    val stLastCompelet = Input(Valid(new RobPtr))

    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(lduNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(lduNum, Bool()))
  })


  private val statusArray = Module(new MemoryStatusArray(entryNum, stuNum, wakeupWidth:Int))
  private val payloadArray = Module(new PayloadArray(new MicroOp, entryNum, issueWidth, "IntegerPayloadArray"))

  private def EnqToEntry(in: MicroOp): MemoryStatusArrayEntry = {
    val stIssueHit = io.stIssued.map(st => st.valid && st.bits === in.cf.waitForRobIdx).reduce(_|_)
    val shouldWait = in.ctrl.fuType === FuType.ldu && in.cf.loadWaitBit && in.cf.waitForRobIdx > io.stLastCompelet.bits && !stIssueHit
    val enqEntry = Wire(new MemoryStatusArrayEntry)
    enqEntry.psrc(0) := in.psrc(0)
    enqEntry.psrc(1) := in.psrc(1)
    enqEntry.srcType(0) := in.ctrl.srcType(0)
    enqEntry.srcType(1) := in.ctrl.srcType(1)
    enqEntry.srcState(0) := in.srcState(0)
    enqEntry.srcState(1) := in.srcState(1)
    enqEntry.pdest := in.pdest
    enqEntry.lpv.foreach(_.foreach(_ := 0.U))
    enqEntry.fuType := in.ctrl.fuType
    enqEntry.rfWen := in.ctrl.rfWen
    enqEntry.fpWen := in.ctrl.fpWen
    enqEntry.robIdx := in.robIdx
    enqEntry.staLoadState := Mux(shouldWait, s_wait_st, s_ready)
    enqEntry.stdState := Mux(in.ctrl.fuType === FuType.stu, s_ready, s_issued)
    enqEntry.waitTarget := in.cf.waitForRobIdx
    enqEntry.isFirstIssue := false.B
    enqEntry.counter := 0.U
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
  statusArray.io.staIssue := io.staIssue
  statusArray.io.stdIssue := io.stdIssue
  statusArray.io.lduIssue := io.lduIssue
  statusArray.io.replay := io.replay
  statusArray.io.stIssued.zip(io.stIssued).foreach({case(a, b) => a := Pipe(b)})
  statusArray.io.wakeup := io.wakeup
  statusArray.io.loadEarlyWakeup := io.loadEarlyWakeup
  statusArray.io.earlyWakeUpCancel := io.earlyWakeUpCancel

  payloadArray.io.write.en := io.enq.valid
  payloadArray.io.write.addr := io.enq.bits.addrOH
  payloadArray.io.write.data := io.enq.bits.data
  private val issueVec = Seq(io.staIssue, io.stdIssue, io.lduIssue)
  private val issueUopVec = Seq(io.staIssueUop, io.stdIssueUop, io.lduIssueUop)
  payloadArray.io.read.zip(issueVec).zip(issueUopVec).foreach({
    case((port, issAddr), issData) =>{
      port.addr := issAddr.bits
      issData := port.data
    }
  })
}

