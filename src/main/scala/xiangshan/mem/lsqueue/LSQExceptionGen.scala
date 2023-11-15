package xiangshan.mem.lsqueue
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.execute.fu.{FuConfig, FuConfigs}
import xiangshan.{ExceptionNO, ExceptionVec, Redirect, XSModule}
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.DCacheBundle
import xs.utils.ParallelOperation

class LSQExceptionInfo (implicit p: Parameters)  extends DCacheBundle{
  val eVec = ExceptionVec()
  val robIdx = new RobPtr
  val vaddr = UInt(VAddrBits.W)
  val uopIdx = UInt(log2Ceil(VLEN + 1).W)
}

class ExceptionSelector(inNum:Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in = Input(Vec(inNum, Valid(new LSQExceptionInfo)))
    val out = Output(Valid(new LSQExceptionInfo))
  })

  private def GetOldest(in0: Valid[LSQExceptionInfo], in1: Valid[LSQExceptionInfo]):Valid[LSQExceptionInfo] = {
    val res = Wire(Valid(new LSQExceptionInfo))
    res.valid := in0.valid | in1.valid
    val in0IsOlder = (in0.bits.robIdx < in1.bits.robIdx) || (in0.bits.robIdx === in1.bits.robIdx && in0.bits.uopIdx < in1.bits.uopIdx)
    val sel = Cat(in1.valid, in0.valid)
    when(sel === 1.U) {
      res.bits := in0.bits
    }.elsewhen(sel === 2.U) {
      res.bits := in1.bits
    }.elsewhen(sel === 3.U) {
      res.bits := Mux(in0IsOlder, in0.bits, in1.bits)
    }.otherwise {
      res.bits := in0.bits
    }
    res
  }
  private val selRes = ParallelOperation(io.in, GetOldest)
  io.out := selRes
}

class LSQExceptionGen(wbInNum:Int, fuCfg:FuConfig)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in = Input(Vec(wbInNum, Valid(new LSQExceptionInfo)))
    val out = Output(Valid(new LSQExceptionInfo))
    val redirect = Input(Valid(new Redirect))
  })
  private val exceptionInfo = RegInit(0.U.asTypeOf(Valid(new LSQExceptionInfo())))

  private val s1EcptReg = io.in.map(i => {
    val res = Wire(Valid(new LSQExceptionInfo))
    val hasExcpt = ExceptionNO.selectByFu(i.bits.eVec, fuCfg).asUInt.orR
    res.valid := RegNext(i.valid && hasExcpt && !i.bits.robIdx.needFlush(io.redirect), false.B)
    res.bits := RegEnable(i.bits, i.valid && hasExcpt)
    res
  })

  private val selector = Module(new ExceptionSelector(wbInNum + 1))
  selector.io.in.zip(s1EcptReg :+ exceptionInfo).foreach({case(a, b) =>
    a.valid := b.valid && !b.bits.robIdx.needFlush(io.redirect)
    a.bits := b.bits
  })

  when(selector.io.out.valid) {
    exceptionInfo := selector.io.out
  }.elsewhen(io.redirect.valid && exceptionInfo.valid && exceptionInfo.bits.robIdx.needFlush(io.redirect)) {
    exceptionInfo.valid := false.B
  }
  io.out := exceptionInfo
}
