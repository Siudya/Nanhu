package xiangshan.backend
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.XSModule

class PipelineRouter[T <: Data](gen:T, vecLen:Int, outNum:Int) extends Module{
  val io = IO(new Bundle{
    val in = Flipped(Vec(vecLen, Decoupled(gen)))
    val out = Vec(outNum, Vec(vecLen, Decoupled(gen)))
    val allowOut = Output(Vec(outNum, Bool()))
    val flush = Input(Bool())
    val holds = Input(Vec(vecLen, Bool()))
  })
  io.out.foreach(o => assert(PopCount(o.map(_.ready)) === 0.U || PopCount(o.map(_.ready)) === vecLen.U))

  private val validRegs = Seq.tabulate(outNum, vecLen)((_,_) => RegInit(false.B))
  private val bitsRegs = Seq.tabulate(outNum, vecLen)((_,_) => Reg(gen))
  private val masterValidReg = RegInit(false.B)
  private val outReady = io.out.map(_.head.ready).reduce(_&_)
  private val allowIn = (!masterValidReg || outReady) && !io.flush
  private val allowOut = Wire(Vec(outNum, Bool()))

  allowOut.zipWithIndex.foreach({case(a, i) =>
    a := io.out.zipWithIndex.filterNot(_._2 == i).map(_._1.head.ready).reduce(_&_)
  })
  io.allowOut := allowOut

  io.in.foreach(_.ready := allowIn)

  private val inValid = io.in.map(_.valid).reduce(_|_)
  when(io.flush){
    masterValidReg := false.B
  }.elsewhen(allowIn){
    masterValidReg := inValid
  }

  validRegs.zip(bitsRegs).foreach({case(vrl, brl) =>
    vrl.zip(brl).zip(io.in).foreach({case((vr, br), in) =>
      when(io.flush){
        vr := false.B
      }.elsewhen(allowIn) {
        vr := in.valid
      }
      when(in.fire){
        br := in.bits
      }
    })
  })

  validRegs.zip(bitsRegs).zip(io.out).foreach({case((vrl, brl), ol) =>
    vrl.zip(brl).zip(ol).zip(io.holds).foreach({case(((vr, br), out), hold) =>
      out.valid := vr & !hold
      out.bits := br
    })
  })

}
