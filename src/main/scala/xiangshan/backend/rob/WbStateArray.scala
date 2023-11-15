package xiangshan.backend.rob
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{ExuOutput, Redirect, XSBundle, XSModule}
class WbReq(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val data = Bool()
}
class WbStateArray(wbNum:Int, redirectNum:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val updateIn = Input(Vec(wbNum, Valid(new WbReq)))
    val redirectIn = Input(Vec(redirectNum, Valid(new WbReq)))
    val out = Output(Vec(RobSize, Bool()))
  })
  private val in = io.updateIn
  private val inr = io.redirectIn
  private val writebacked = RegInit(VecInit(Seq.fill(RobSize)(false.B)))
  for((wb, idx) <- writebacked.zipWithIndex) {
    val inSel = in.map(r => r.valid && r.bits.robIdx.value === idx.U)
    val inData = Mux1H(inSel, in.map(_.bits.data))
    val inHit = Cat(inSel).orR
    val inrSel = inr.map(r => r.valid && r.bits.robIdx.value === idx.U)
    val inrData = Mux1H(inrSel, inr.map(_.bits.data))
    val inrHit = Cat(inrSel).orR
    wb := Mux(inrHit, inrData, Mux(inHit, inData, wb))
  }

  io.out.zip(writebacked).foreach({case(a, b) => a := b})

  private var upidx = 0
  private var rdidx = 0

  def update(v:Bool, addr:RobPtr, data:Bool):Unit = {
    this.io.updateIn(upidx).valid := v
    this.io.updateIn(upidx).bits.robIdx := addr
    this.io.updateIn(upidx).bits.data := data
    upidx = upidx + 1
  }

  def redirect(v:Bool, addr:RobPtr, data:Bool):Unit = {
    this.io.redirectIn(rdidx).valid := v
    this.io.redirectIn(rdidx).bits.robIdx := addr
    this.io.redirectIn(rdidx).bits.data := data
    rdidx = rdidx + 1
  }
}
