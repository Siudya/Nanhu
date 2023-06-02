package xiangshan.backend.issue
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.Redirect
import xiangshan.backend.rob.RobPtr
import xs.utils.PickOneLow
class TokenAllocatorEntry(pdestWidth:Int)(implicit val p: Parameters) extends Bundle{
  val robPtr = new RobPtr
  val pdest = UInt(pdestWidth.W)
}
class TokenAllocator(pdestWidth:Int, tokenNum:Int)(implicit val p: Parameters) extends Module{
  val io = IO(new Bundle{
    val alloc = Input(Valid(new TokenAllocatorEntry(pdestWidth)))
    val allow = Output(Bool())
    val release = Input(Valid(UInt(pdestWidth.W)))
    val redirect = Input(Valid(new Redirect))
  })
  private val valids = RegInit(VecInit(Seq.fill(tokenNum)(false.B)))
  private val payload = Reg(Vec(tokenNum, new TokenAllocatorEntry(pdestWidth)))

  private val emptyToken = PickOneLow(valids)
  io.allow := emptyToken.valid

  private val allocEnables = Mux(emptyToken.valid, emptyToken.bits, 0.U)
  valids.zip(payload).zip(allocEnables.asBools).foreach({
    case((v, d), en) =>
      when(v && (d.robPtr.needFlush(io.redirect) || (io.release.valid && d.pdest === io.release.bits))){
        v := false.B
      }.elsewhen(io.alloc.valid && en){
        v := true.B
      }

      when(io.alloc.valid && en){
        d := io.alloc.bits
      }
      assert(Mux(en, !v, true.B))
  })
}
