package xiangshan.vector.vbackend.vissue.vrs

import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.issue._
import chisel3._
import chisel3.util._
import xiangshan.{MicroOp, Redirect, SrcState, SrcType}

class VrsBank(entryNum:Int, issueWidth:Int, wakeupWidth:Int, loadUnitNum:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))

    val selectInfo = Output(Vec(entryNum, Valid(new VrsSelectInfo)))
    val alloc = Output(Bool())

    val enq = Input(Valid(new MicroOp))

    val issueAddr = Input(Vec(issueWidth, Valid(UInt(entryNum.W))))
    val issueUop = Output(Vec(issueWidth, Valid(new MicroOp)))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
  })

  private val statusArray = Module(new VrsStatusArray(entryNum, issueWidth, wakeupWidth, loadUnitNum))
  private val payloadArray = Module(new PayloadArray(new MicroOp, entryNum, issueWidth, "VrsPayloadArray"))

  private def EnqToEntry(in: MicroOp): VrsStatusArrayEntry = {
    val dontCareDest = in.vCsrInfo.vta(0) && Mux(in.vctrl.vm, in.vCsrInfo.vma(0), true.B)
    val enqEntry = Wire(new VrsStatusArrayEntry)
    enqEntry.psrc.take(2).zip(in.psrc.take(2)).foreach(elm => elm._1 := elm._2)
    enqEntry.srcType.take(2).zip(in.ctrl.srcType.take(2)).foreach(elm => elm._1 := elm._2)
    enqEntry.srcState.take(2).zip(in.srcState.take(2)).zip(in.ctrl.srcType.take(2)).foreach(elm =>
      elm._1._1 := Mux(SrcType.needWakeup(elm._2), elm._1._2, SrcState.rdy)
    )
    enqEntry.psrc(2) := in.psrc(2)
    when(SrcType.needWakeup(in.ctrl.srcType(2))){
      enqEntry.srcType(2) := in.ctrl.srcType(2)
      enqEntry.srcState(2) := in.srcState(2)
    }.elsewhen(in.ctrl.vdWen){
      enqEntry.srcType(2) := SrcType.vec
      enqEntry.srcState(2) := Mux(dontCareDest, SrcState.rdy, in.srcState(2))
    }.otherwise{
      enqEntry.srcType(2) := SrcType.DC
      enqEntry.srcState(2) := SrcState.rdy
    }

    enqEntry.psrc(3) := in.vm
    enqEntry.srcType(3) := SrcType.vec
    enqEntry.srcState(3) := Mux(in.vctrl.vm, in.vmState, SrcState.rdy)

    enqEntry.fuType := in.ctrl.fuType
    enqEntry.robIdx := in.robIdx
    enqEntry.isOrdered := in.vctrl.ordered
    enqEntry.uopIdx := in.uopIdx(2, 0)
    enqEntry
  }

  statusArray.io.redirect := io.redirect
  io.selectInfo := statusArray.io.selectInfo
  io.alloc := statusArray.io.alloc
  statusArray.io.enq.valid := io.enq.valid
  statusArray.io.enq.bits := EnqToEntry(io.enq.bits)
  when(io.enq.valid){assert(statusArray.io.enq.bits.uopIdx <= 7.U)}
  statusArray.io.issue := io.issueAddr
  statusArray.io.wakeup := io.wakeup

  payloadArray.io.write.en := io.enq.valid
  payloadArray.io.write.addr := statusArray.io.enqAddr
  payloadArray.io.write.data := io.enq.bits
  payloadArray.io.read.zip(io.issueAddr).zip(io.issueUop).foreach({
    case((port, iAddr), iData) =>{
      port.addr := iAddr.bits
      iData.bits := port.data
      iData.valid := iAddr.valid
    }
  })
}
