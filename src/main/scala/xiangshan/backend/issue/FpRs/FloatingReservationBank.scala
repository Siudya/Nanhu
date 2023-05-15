package xiangshan.backend.issue.FpRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.fu.fpu.FMAMidResult
import xiangshan.{MicroOp, Redirect, XSModule}
import xiangshan.backend.issue.{EarlyWakeUpInfo, PayloadArray, SelectInfo, WakeUpInfo}

class FloatingReservationBank(entryNum:Int, issueWidth:Int, wakeupWidth:Int, loadUnitNum:Int)(implicit p: Parameters) extends XSModule{
  private val rsMidStateWidth = (new FMAMidResult).getWidth - XLEN
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))

    val selectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val allocateInfo = Output(UInt(entryNum.W))

    val enq = Input(Valid(new Bundle {
      val addrOH = UInt(entryNum.W)
      val data = new MicroOp
    }))

    val midResultEnq = Input(Valid(new Bundle {
      val addrOH = UInt(entryNum.W)
      val data = UInt(rsMidStateWidth.W)
    }))

    val issueAddr = Input(Vec(issueWidth, Valid(UInt(entryNum.W))))
    val issueUop = Output(Vec(issueWidth, Valid(new MicroOp)))
    val issueMidResult = Output(Vec(issueWidth, UInt(rsMidStateWidth.W)))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val midResultReceived = Input(Valid(UInt(entryNum.W)))
  })

  private val statusArray = Module(new FloatingStatusArray(entryNum, issueWidth, wakeupWidth, loadUnitNum))
  private val payloadArray = Module(new PayloadArray(new MicroOp, entryNum, issueWidth, "FloatingPayloadArray"))
  private val midStatePayloadArray = Module(new PayloadArray(UInt(rsMidStateWidth.W), entryNum, issueWidth, "FloatingFmaMidStatePayloadArray"))

  private def EnqToEntry(in: MicroOp): FloatingStatusArrayEntry = {
    val enqEntry = Wire(new FloatingStatusArrayEntry)
    enqEntry.psrc := in.psrc
    enqEntry.srcType := in.ctrl.srcType
    enqEntry.srcState := in.srcState
    enqEntry.pdest := in.pdest
    enqEntry.lpv.foreach(_.foreach(_ := 0.U))
    enqEntry.fuType := in.ctrl.fuType
    enqEntry.rfWen := in.ctrl.rfWen
    enqEntry.fpWen := in.ctrl.fpWen
    enqEntry.robIdx := in.robIdx
    enqEntry.state := EntryState.s_idle
    enqEntry.isFma := in.ctrl.fpu.ren3
    enqEntry
  }

  statusArray.io.redirect := io.redirect
  io.selectInfo := statusArray.io.selectInfo
  io.allocateInfo := statusArray.io.allocateInfo
  statusArray.io.enq.valid := io.enq.valid
  statusArray.io.enq.bits.addrOH := io.enq.bits.addrOH
  statusArray.io.enq.bits.data := EnqToEntry(io.enq.bits.data)
  statusArray.io.issue := io.issueAddr
  statusArray.io.wakeup := io.wakeup
  statusArray.io.loadEarlyWakeup := io.loadEarlyWakeup
  statusArray.io.earlyWakeUpCancel := io.earlyWakeUpCancel
  statusArray.io.midResultReceived := io.midResultReceived

  payloadArray.io.write.en := io.enq.valid
  payloadArray.io.write.addr := io.enq.bits.addrOH
  payloadArray.io.write.data := io.enq.bits.data
  payloadArray.io.read.zip(io.issueAddr).zip(io.issueUop).foreach({
    case((port, iAddr), iData) =>{
      port.addr := iAddr.bits
      iData.bits := port.data
      iData.valid := iAddr.valid
    }
  })

  midStatePayloadArray.io.write.en := io.midResultEnq.valid
  midStatePayloadArray.io.write.addr := io.midResultEnq.bits.addrOH
  midStatePayloadArray.io.write.data := io.midResultEnq.bits.data
  midStatePayloadArray.io.read.zip(io.issueAddr).zip(io.issueMidResult).foreach({
    case ((port, iAddr), iData) => {
      port.addr := iAddr.bits
      iData := port.data
    }
  })
}
