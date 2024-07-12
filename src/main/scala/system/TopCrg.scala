package system
import chisel3._
import org.chipsalliance.cde.config.Parameters
import xs.utils.ClockGate
import xs.utils.dft.{EdtFuncBundle, EdtWrapper, HasIjtag, SIB, SOCC}
import xs.utils.{DFTResetSignals, ResetGen}
class TopCrgDftBundle(ci:Int, co:Int) extends Bundle {
  val occ_shift:Bool = Input(Bool())
  val scan_en: Bool = Input(Bool())
  val edt: EdtFuncBundle = new EdtFuncBundle(ci, co)
  val rstCtl: DFTResetSignals = Input(new DFTResetSignals)
}

class TopCrg(edtCi:Int, edtCo:Int, edtScan:Int, edtRange:(Int, Int))(implicit p: Parameters) extends Module with HasIjtag{
  val mName = "TopCrg"
  val io = IO(new Bundle {
    val miscClock = Output(Clock())
    val periClock = Output(Clock())
    val sysReset = Output(AsyncReset())
  })
  val dfx = IO(new TopCrgDftBundle(edtCi, edtCo))
  private val sib = Module(new SIB)
  private val rstSync = Module(new ResetGen(2))
  private val fsSocc = Module(new SOCC)
  private val hsSocc = Module(new SOCC)
  private val edt = Module(new EdtWrapper(
    mName = "TopEdtWrapper",
    instName = "edt",
    ciw = edtCi,
    cow = edtCo,
    sw = edtScan,
    chainRange = edtRange
  ))
  rstSync.dft := dfx.rstCtl
  io.sysReset := rstSync.o_reset
  private val periClkCtrl = withClockAndReset(clock, rstSync.raw_reset) {
    val gt_ff = RegInit(true.B)
    gt_ff := ~gt_ff
    gt_ff
  }
  fsSocc.io.scan := DontCare
  fsSocc.io.scan.en := dfx.scan_en
  fsSocc.io.clock := clock
  fsSocc.io.shiftOnlyMode := dfx.occ_shift
  fsSocc.io.clkCtrl := true.B
  io.miscClock := fsSocc.io.outClock

  hsSocc.io.scan := DontCare
  hsSocc.io.scan.en := dfx.scan_en
  hsSocc.io.clock := clock
  hsSocc.io.shiftOnlyMode := dfx.occ_shift
  hsSocc.io.clkCtrl := periClkCtrl
  io.periClock := hsSocc.io.outClock

  edt.io.scan_en := dfx.scan_en
  edt.io.ci := dfx.edt.in_channels
  dfx.edt.out_channels := edt.io.co
  edt.io.update := dfx.edt.update
  edt.io.atpg_clock := clock

  makeChain(Seq(ijtag, sib.ijtag, edt.io.ijtag))
  makeChain(Seq(sib.host, fsSocc.ijtag, hsSocc.ijtag))
}
