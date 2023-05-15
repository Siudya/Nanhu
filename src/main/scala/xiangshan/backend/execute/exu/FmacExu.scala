package xiangshan.backend.execute.exu
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.backend.execute.fu.fpu.FMA

class FmacExu(id:Int, complexName:String)(implicit p:Parameters) extends BasicExu{
  private val cfg = ExuConfig(
    name = "FmacExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.fmacCfg),
    exuType = ExuType.fmac,
    speculativeWakeup = true
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)
  lazy val module = new FmacExuImpl(this, cfg)
}
class FmacExuImpl(outer:FmacExu, exuCfg:ExuConfig)(implicit p:Parameters) extends BasicExuImpl(outer){
  private val fmac = Module(new FMA)
  private val issuePort = outer.issueNode.in.head._1
  private val writebackPort = outer.writebackNode.out.head._1

  fmac.io.redirectIn := redirectIn
  fmac.io.in.valid := issuePort.issue.valid && issuePort.issue.bits.uop.ctrl.fuType === exuCfg.fuConfigs.head.fuType
  fmac.io.in.bits.uop := issuePort.issue.bits.uop
  fmac.io.in.bits.src := issuePort.issue.bits.src
  issuePort.issue.ready := fmac.io.in.ready
  fmac.rm := issuePort.issue.bits.uop.ctrl.fpu.rm
  fmac.midResult <> issuePort.fmaMidState
  issuePort.fuInFire := DontCare

  writebackPort.valid := fmac.io.out.valid
  fmac.io.out.ready := true.B
  writebackPort.bits.uop := fmac.io.out.bits.uop
  writebackPort.bits.data := fmac.io.out.bits.data
  writebackPort.bits.fflags := fmac.fflags
  writebackPort.bits.redirect := DontCare
  writebackPort.bits.redirectValid := false.B
}
