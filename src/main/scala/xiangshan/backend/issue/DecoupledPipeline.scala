package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.fu.fpu.FMAMidResult
import xiangshan.{MicroOp, Redirect, XSModule}
import xs.utils.Assertion.xs_assert
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, LogicShiftRight}
sealed class TwoEntryQueuePtr extends CircularQueuePtr[TwoEntryQueuePtr](entries = 2) with HasCircularQueuePtrHelper
sealed class PipelineEntry(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends Bundle{
  val uop = new MicroOp
  val fmaMidStateIssue: Valid[FMAMidResult] = Valid(new FMAMidResult)
  val fmaWaitForAdd: Bool = Bool()
  val bankIdxOH: UInt = UInt(bankIdxWidth.W)
  val entryIdxOH: UInt = UInt(entryIdxWidth.W)
}
class DecoupledPipeline(implementQueue:Boolean, bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Flipped(DecoupledIO(new PipelineEntry(bankIdxWidth, entryIdxWidth)))
    val deq = DecoupledIO(new PipelineEntry(bankIdxWidth, entryIdxWidth))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })
  if(implementQueue) {
    val mem = Reg(Vec(2, new PipelineEntry(bankIdxWidth, entryIdxWidth)))
    val enqPtr = RegInit(0.U.asTypeOf(new TwoEntryQueuePtr))
    val deqPtr = RegInit(0.U.asTypeOf(new TwoEntryQueuePtr))
    val full = enqPtr.value === deqPtr.value && enqPtr.flag =/= deqPtr.flag
    val empty = enqPtr.value === deqPtr.value && enqPtr.flag === deqPtr.flag
    val enqFire = io.enq.fire
    val deqFire = io.deq.fire

    val shouldBeKilled = Wire(Vec(2, Bool()))
    mem.map(_.uop).zip(shouldBeKilled).foreach({case(u, k) =>
      val cancelHit = u.lpv.zip(io.earlyWakeUpCancel).map({ case (l, c) => l(0) && c }).reduce(_ || _)
      val flushHit = u.robIdx.needFlush(io.redirect)
      k := cancelHit | flushHit
    })

    io.enq.ready := !full
    io.deq.valid := !empty && !shouldBeKilled(deqPtr.value)
    val outData = mem(deqPtr.value)
    io.deq.bits := outData
    io.deq.bits.uop.lpv.zip(outData.uop.lpv).foreach({case(a,b) => a := LogicShiftRight(b, 1)})

    mem.flatMap(_.uop.lpv).foreach(l =>{
      when(l.orR){
        l := LogicShiftRight(l, 1)
      }
    })

    when(full && shouldBeKilled((enqPtr - 1.U).value)){
      enqPtr := enqPtr - 1.U
    }.elsewhen(enqFire){
      mem(enqPtr.value) := io.enq.bits
      enqPtr := enqPtr + 1.U
    }
    when(deqFire || (!empty && shouldBeKilled(deqPtr.value))) {
      deqPtr := deqPtr + 1.U
    }
  } else {
    //ready should be true all the time
    io.enq.ready := io.deq.ready
    xs_assert(io.deq.ready)
    val deqValidDriverReg = RegNext(io.enq.valid, false.B)
    val deqDataDriverReg = RegEnable(io.enq.bits, io.enq.valid)
    val shouldBeFlushed = deqDataDriverReg.uop.robIdx.needFlush(io.redirect)
    val shouldBeCanceled = deqDataDriverReg.uop.lpv.zip(io.earlyWakeUpCancel).map({case(l,c) => l(0) && c}).reduce(_||_)
    io.deq.valid := deqValidDriverReg && !shouldBeFlushed && !shouldBeCanceled
    io.deq.bits := deqDataDriverReg
    io.deq.bits.uop.lpv.zip(deqDataDriverReg.uop.lpv).foreach({case(a,b) => a := LogicShiftRight(b, 1)})
  }

  xs_assert(Mux(io.deq.valid, !io.deq.bits.uop.robIdx.needFlush(io.redirect), true.B))
}