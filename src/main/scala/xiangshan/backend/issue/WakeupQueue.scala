package xiangshan.backend.issue
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{Redirect, SrcType, XSModule}
import xs.utils.LogicShiftRight
class WakeupQueue(latency:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val in = Input(Valid(new WakeUpInfo))
    val out = Output(Valid(new WakeUpInfo))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })
  require(latency > 0)
  private def DelayInput(in:Valid[WakeUpInfo], l:Int): Valid[WakeUpInfo] = {
    val res = Wire(Valid(new WakeUpInfo))
    val realIn = if(l == 1) in else DelayInput(in, l - 1)
    val resValidReg = RegNext(realIn.valid, false.B)
    val resDataReg = RegEnable(realIn.bits, realIn.valid)
    val shouldBeFlushed = resDataReg.robPtr.needFlush(io.redirect)
    val shouldBeCanceled = resDataReg.lpv.zip(io.earlyWakeUpCancel).map({ case (l, c) => l(0) && c }).reduce(_ || _)
    res.valid := resValidReg && !shouldBeCanceled && !shouldBeFlushed
    res.bits := resDataReg
    res.bits.lpv.zip(resDataReg.lpv).foreach({case(a,b) => a := LogicShiftRight(b, 1)})
    res
  }
  io.out := DelayInput(io.in, latency)
  assert(Mux(io.out.valid, !io.out.bits.robPtr.needFlush(io.redirect), true.B))
}

object WakeupQueue {
  def apply(in:DecoupledIO[SelectResp], latency:Int, redirect:Valid[Redirect], cancel:Vec[Bool], p: Parameters):Valid[WakeUpInfo] = {
    val res = Wire(Valid(new WakeUpInfo()(p)))
    if(latency > 0) {
      val wakeupQueue = Module(new WakeupQueue(latency)(p))
      wakeupQueue.io.in.valid := in.fire
      wakeupQueue.io.earlyWakeUpCancel := cancel
      wakeupQueue.io.in.bits.lpv := in.bits.info.lpv
      wakeupQueue.io.in.bits.robPtr := in.bits.info.robPtr
      wakeupQueue.io.in.bits.pdest := in.bits.info.pdest
      wakeupQueue.io.in.bits.destType := Mux(in.bits.info.fpWen, SrcType.fp, Mux(in.bits.info.rfWen, SrcType.reg, SrcType.default))
      wakeupQueue.io.redirect := redirect
      res := wakeupQueue.io.out
    } else {
      res.valid := in.fire
      res.bits.lpv := in.bits.info.lpv
      res.bits.robPtr := in.bits.info.robPtr
      res.bits.pdest := in.bits.info.pdest
      res.bits.destType := Mux(in.bits.info.fpWen, SrcType.fp, Mux(in.bits.info.rfWen, SrcType.reg, SrcType.default))
    }
    res
  }
}