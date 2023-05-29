package xiangshan.backend.execute.exu
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.fu.mdu.DividerWrapper
import xiangshan.backend.execute.fu.{FuConfigs, FuOutput}
import xiangshan.{ExuOutput, HasXSParameter}
import xs.utils.Assertion.xs_assert
import xs.utils.PickOneHigh

class DivExu(id:Int, complexName:String, val bypassInNum:Int)(implicit p:Parameters) extends BasicExu with HasXSParameter{
  private val cfg = ExuConfig(
    name = "DivExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.divCfg, FuConfigs.divCfg, FuConfigs.divCfg),
    exuType = ExuType.div,
    needToken = true
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)
  lazy val module = new DivExuImpl(this, cfg)
}

class DivExuImpl(outer:DivExu, exuCfg:ExuConfig) extends BasicExuImpl(outer) with HasXSParameter{
  val io = IO(new Bundle {
    val bypassIn = Input(Vec(outer.bypassInNum, Valid(new ExuOutput)))
  })
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1
  private val divs = Seq.fill(exuCfg.fuConfigs.length)(Module(new DividerWrapper(XLEN)))
  private val outputArbiter = Module(new Arbiter(new FuOutput(XLEN), exuCfg.fuConfigs.length))

  private val finalIssueSignals = bypassSigGen(io.bypassIn, issuePort, outer.bypassInNum > 0)

  private val divSel = PickOneHigh(Cat(divs.map(_.io.in.ready).reverse))
  issuePort.issue.ready := true.B
  for(((div, en), arbIn) <- divs.zip(Mux(divSel.valid, divSel.bits, 0.U).asBools).zip(outputArbiter.io.in)){
    div.io.redirectIn := redirectIn
    div.io.in.valid := finalIssueSignals.valid & en & finalIssueSignals.bits.uop.ctrl.fuType === exuCfg.fuConfigs.head.fuType
    div.io.in.bits.uop := finalIssueSignals.bits.uop
    div.io.in.bits.src := finalIssueSignals.bits.src
    arbIn <> div.io.out
    xs_assert(Mux(div.io.in.valid, div.io.in.ready, true.B))
  }
  outputArbiter.io.out.ready := true.B
  writebackPort.bits := DontCare
  writebackPort.valid := outputArbiter.io.out.valid
  writebackPort.bits.uop := outputArbiter.io.out.bits.uop
  writebackPort.bits.data := outputArbiter.io.out.bits.data
}