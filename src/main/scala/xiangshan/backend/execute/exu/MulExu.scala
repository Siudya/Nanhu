package xiangshan.backend.execute.exu
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.backend.execute.fu.bku.Bku
import xiangshan.backend.execute.fu.fpu.IntToFP
import xiangshan.backend.execute.fu.mdu.{ArrayMultiplier, MDUOpType}
import xiangshan.{ExuOutput, HasXSParameter}
import xs.utils.{LookupTree, ParallelMux, SignExt, ZeroExt}

class MulExu(id:Int, complexName:String, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu{
  private val cfg  = ExuConfig(
    name = "MulExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.mulCfg, FuConfigs.bkuCfg, FuConfigs.i2fCfg),
    exuType = ExuType.mul
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)

  lazy val module = new MulExuImpl(this, cfg)
}
class MulExuImpl(outer:MulExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer) with HasXSParameter{
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
    val bypassOut = Output(Valid(new ExuOutput))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  private val mul = Module(new ArrayMultiplier(XLEN + 1)(p))
  private val bku = Module(new Bku)
  private val i2f = Module(new IntToFP)

  issuePort.issue.ready := true.B
  private val finalIssueSignals = bypassSigGen(io.bypassIn :+ io.bypassOut, issuePort, true)
  private val fuSeq = Seq(mul, bku, i2f)
  fuSeq.zip(exuCfg.fuConfigs).foreach({case(m, cfg) =>
    m.io.redirectIn := redirectIn
    m.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === cfg.fuType
    m.io.in.bits.uop := finalIssueSignals.bits.uop
    m.io.in.bits.src := finalIssueSignals.bits.src
    m.io.out.ready := true.B
    assert(Mux(m.io.in.valid, m.io.in.ready, true.B))
  })
  i2f.rm := finalIssueSignals.bits.uop.ctrl.fpu.rm

  private val (func, src1, src2) = (
    finalIssueSignals.bits.uop.ctrl.fuOpType,
    finalIssueSignals.bits.src(0)(XLEN - 1, 0),
    finalIssueSignals.bits.src(1)(XLEN - 1, 0)
  )
  private val op = MDUOpType.getMulOp(func)
  private val signext = SignExt(_: UInt, XLEN + 1)
  private val zeroext = ZeroExt(_: UInt, XLEN + 1)
  private val mulInputFuncTable = List(
    MDUOpType.mul -> (zeroext, zeroext),
    MDUOpType.mulh -> (signext, signext),
    MDUOpType.mulhsu -> (signext, zeroext),
    MDUOpType.mulhu -> (zeroext, zeroext)
  )

  mul.io.in.bits.src(0) := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1, 0), p._2._1(src1)))
  )
  when(func(3)) {
    mul.io.in.bits.src(0) := src1(6, 0)
  }
  mul.io.in.bits.src(1) := LookupTree(
    op,
    mulInputFuncTable.map(p => (p._1(1, 0), p._2._2(src2)))
  )

  private val isW = MDUOpType.isW(func)
  private val isH = MDUOpType.isH(func)
  mul.ctrl.isW := isW
  mul.ctrl.isHi := isH
  mul.ctrl.sign := DontCare

  private val fuOut = fuSeq.map(m =>{
    val out = WireInit(m.io.out)
    out.valid := RegNext(m.io.out.valid, false.B)
    out.bits.uop := RegEnable(m.io.out.bits.uop, m.io.out.valid)
    out
  })

  private val outSel = fuOut.map(_.valid)
  private val outData = fuOut.map(_.bits)
  private val finalData = ParallelMux(outSel, outData)
  writebackPort := DontCare
  writebackPort.valid := outSel.reduce(_||_) && !finalData.uop.robIdx.needFlush(redirectIn)
  writebackPort.bits.uop := finalData.uop
  writebackPort.bits.data := finalData.data
  writebackPort.bits.fflags := i2f.fflags
  io.bypassOut.valid := writebackPort.valid && writebackPort.bits.uop.ctrl.rfWen && writebackPort.bits.uop.pdest =/= 0.U
  io.bypassOut.bits := writebackPort.bits

  assert(mul.io.in.ready)
  assert(bku.io.in.ready)
  assert(i2f.io.in.ready)
  assert(PopCount(outSel) === 1.U || PopCount(outSel) === 0.U)
}
