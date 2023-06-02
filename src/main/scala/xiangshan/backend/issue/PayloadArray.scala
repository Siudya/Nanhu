package xiangshan.backend.issue
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.XSModule
class PayloadArrayReadIO[T <: Data](gen:T, entryNum:Int) extends Bundle {
  val addr = Input(UInt(entryNum.W))
  val data = Output(gen)
}

class PayloadArrayWriteIO[T <: Data](gen:T, entryNum:Int) extends Bundle {
  val en: Bool = Input(Bool())
  val addr: UInt = Input(UInt(entryNum.W))
  val data: T = Input(gen)
}

class PayloadArray[T <: Data](gen:T, entryNum:Int, deqNum:Int, name:String)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val read = Vec(deqNum, new PayloadArrayReadIO(gen, entryNum))
    val write = new PayloadArrayWriteIO(gen, entryNum)
  })
  override val desiredName:String = name

  private val payload = Reg(Vec(entryNum, gen))

  // read ports
  io.read.foreach(r => {r.data := Mux1H(r.addr.asBools, payload)})

  // write ports
  for (idx <- 0 until entryNum) {
    val wen = io.write.en & io.write.addr(idx)
    val data = io.write.data
    when(wen){
      payload(idx) := data
    }
  }

  io.read.foreach(r => assert(PopCount(r.addr) === 1.U))
  assert(Mux(io.write.en, PopCount(io.write.addr) === 1.U, true.B))
}