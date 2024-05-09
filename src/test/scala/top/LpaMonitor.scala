package top
import chisel3._

class LpaMonitor extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Bool())
    val stop = Input(Bool())
    val reset = Input(Bool())
  })
}
