package xiangshan.backend.execute.exu
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.fu.fence._
import xiangshan.backend.execute.fu.jmp._
import xiangshan.backend.execute.fu.{FUWithRedirect, FuConfigs, FunctionUnit}
import xiangshan.{SfenceBundle, _}
import xs.utils.Assertion.xs_assert
import xs.utils.ParallelMux

class FenceIO(implicit p: Parameters) extends XSBundle {
  val sfence = Output(new SfenceBundle)
  val fencei = Output(Bool())
  val sbuffer = new FenceToSbuffer
}

class JmpCsrExu (id:Int, complexName:String, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu{
  private val cfg = ExuConfig(
    name = "JmpCsrExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.jmpCfg, FuConfigs.fenceCfg, FuConfigs.mouCfg, FuConfigs.csrCfg),
    exuType = ExuType.jmp
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)
  override lazy val module = new JmpCsrExuImpl(this, cfg)
}
class FakeMou()(implicit p:Parameters) extends FunctionUnit(p(XSCoreParamsKey).XLEN) {
  val issueToMou = IO(Decoupled(new ExuInput))
  val writebackFromMou = IO(Flipped(Decoupled(new ExuOutput)))
  io.in.ready := issueToMou.ready
  issueToMou.valid := io.in.valid
  issueToMou.bits.src := io.in.bits.src
  issueToMou.bits.uop := io.in.bits.uop
  io.out.valid := writebackFromMou.valid
  io.out.bits := writebackFromMou.bits
  writebackFromMou.ready := io.out.ready
}
class FakeCsr(implicit p:Parameters) extends FUWithRedirect(p(XSCoreParamsKey).XLEN){
  io.in.ready := true.B
  io.out := DontCare
  redirectOutValid := false.B
  redirectOut := DontCare
}

class JmpCsrExuImpl(outer:JmpCsrExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer) {
  val io = IO(new Bundle{
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
    val fenceio = new FenceIO
    val issueToMou = Decoupled(new ExuInput)
    val writebackFromMou = Flipped(Decoupled(new ExuOutput))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1
  private val fence = Module(new Fence)
  private val jmp = Module(new Jump)
  private val mou = Module(new FakeMou)
  private val csr = Module(new FakeCsr)

  issuePort.issue.ready := true.B
  issuePort.fmaMidState.out := DontCare
  issuePort.fuInFire := DontCare

  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)
  private val fuSeq = Seq(jmp, fence, mou, csr)
  fuSeq.zip(exuCfg.fuConfigs).foreach({ case (m, cfg) =>
    m.io.redirectIn := redirectIn
    m.io.in.valid := finalIssueSignals.valid && finalIssueSignals.bits.uop.ctrl.fuType === cfg.fuType
    m.io.in.bits.uop := finalIssueSignals.bits.uop
    m.io.in.bits.src := finalIssueSignals.bits.src
    m.io.out.ready := true.B

    val isJmp = finalIssueSignals.bits.uop.ctrl.fuType === FuType.jmp
    val isExclusive = finalIssueSignals.bits.uop.ctrl.noSpecExec && finalIssueSignals.bits.uop.ctrl.blockBackward
    xs_assert(Mux(m.io.in.valid, m.io.in.ready, true.B))
    xs_assert(isJmp || isExclusive)
  })

  private val fuOut = fuSeq.map(_.io.out)
  private val outSel = fuOut.map(_.fire)
  private val outData = fuOut.map(_.bits)
  private val finalData = ParallelMux(outSel, outData)

  writebackPort := DontCare
  writebackPort.valid := outSel.reduce(_ || _)
  writebackPort.bits.uop := finalData.uop
  writebackPort.bits.data := finalData.data

  xs_assert(PopCount(outSel) === 1.U || PopCount(outSel) === 0.U)

  io.fenceio.sfence := fence.sfence
  io.fenceio.fencei := fence.fencei
  io.fenceio.sbuffer <> fence.toSbuffer

  io.issueToMou <> mou.issueToMou
  io.writebackFromMou <> mou.writebackFromMou

  writebackPort.bits.fflags := DontCare
  private val redirectValids = VecInit(Seq(csr.redirectOutValid, jmp.redirectOutValid, fence.redirectOutValid))
  private val redirectBits = Seq(csr.redirectOut, jmp.redirectOut, fence.redirectOut)
  writebackPort.bits.redirect := Mux1H(redirectValids, redirectBits)
  writebackPort.bits.redirectValid := redirectValids.reduce(_ || _)

  xs_assert(PopCount(redirectValids.asUInt) === 1.U || PopCount(redirectValids.asUInt) === 0.U)
  //TODO: this signals should connect to csr
  fence.disableSfence := DontCare
}
