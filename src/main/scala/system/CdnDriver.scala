package system
import chisel3._
import chisel3.util._
import xs.utils.dft._

class CdnDriver extends RawModule with HasIjtag {
  val mName = "CdnDriver"
  val io = IO(new Bundle{
    val shiftOnlyMode = Input(Bool())
    val scanEn = Input(Bool())
    val funcClk = Input(Clock())
    val atpgClk = Input(Clock())
    val outClock = Output(Clock())
  })
  dontTouch(io.scanEn)
  private val mocc = Module(new MOCC)
  private val sib = Module(new SIB)
  mocc.io.scanEn := io.scanEn
  mocc.io.shiftOnlyMode := io.shiftOnlyMode
  mocc.io.fastClock := io.funcClk
  mocc.io.slowClock := io.atpgClk
  io.outClock := mocc.io.outClock
  makeChain(Seq(ijtag, sib.ijtag))
  makeChain(Seq(sib.host, mocc.ijtag))
}
