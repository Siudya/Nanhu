package system
import chisel3._
import xs.utils.ClockGate
import xs.utils.{DFTResetSignals, ResetGen}

class TopCrg extends Module {
  val io = IO(new Bundle {
    val dfx = Input(new DFTResetSignals)
    val sysClock = Output(Clock())
    val periClock = Output(Clock())
    val sysReset = Output(AsyncReset())
  })
  private val rstSync = Module(new ResetGen(2))
  rstSync.dft := io.dfx
  io.sysReset := rstSync.o_reset
  withClockAndReset(clock, rstSync.raw_reset) {
    val gt_ff = RegInit(true.B)
    gt_ff := ~gt_ff
    val clkGt = Module(new ClockGate)
    clkGt.io.TE := false.B
    clkGt.io.E := gt_ff
    clkGt.io.CK := clock
    io.sysClock := clock
    io.periClock := clkGt.io.Q
  }
}
