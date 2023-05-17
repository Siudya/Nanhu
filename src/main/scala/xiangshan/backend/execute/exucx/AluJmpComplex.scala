package xiangshan.backend.execute.exucx
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.backend.execute.exu.{AluExu, ExuType, FenceIO, JmpCsrExu}
import xiangshan.backend.execute.fu.csr.CSRFileIO
import xiangshan.{ExuInput, ExuOutput, FuType}
import xs.utils.Assertion.xs_assert

class AluJmpComplex(id: Int, bypassNum:Int)(implicit p:Parameters) extends BasicExuComplex{
  val alu = LazyModule(new AluExu(id, "AluJmpComplex", bypassNum))
  val jmp = LazyModule(new JmpCsrExu(id, "AluJmpComplex", bypassNum))
  alu.issueNode :*= issueNode
  jmp.issueNode :*= issueNode
  writebackNode :=* alu.writebackNode
  writebackNode :=* jmp.writebackNode
  lazy val module = new BasicExuComplexImp(this, bypassNum){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == 2)
    private val issueIn = issueNode.in.head._1
    private val issueAlu = issueNode.out.filter(_._2._2.exuType == ExuType.alu).head._1
    private val issueJmp = issueNode.out.filter(_._2._2.exuType == ExuType.jmp).head._1
    val io = IO(new Bundle {
      val fenceio = new FenceIO
      val csrio = new CSRFileIO
      val issueToMou = Decoupled(new ExuInput)
      val writebackFromMou = Flipped(Decoupled(new ExuOutput))
    })

    issueAlu <> issueIn
    alu.module.io.bypassIn := bypassIn
    alu.module.redirectIn := redirectIn

    issueJmp <> issueIn
    jmp.module.io.bypassIn := bypassIn
    jmp.module.redirectIn := redirectIn

    jmp.module.io.fenceio <> io.fenceio
    jmp.module.io.csrio <> io.csrio
    io.issueToMou <> jmp.module.io.issueToMou
    io.writebackFromMou <> jmp.module.io.writebackFromMou

    issueIn.fuInFire := DontCare
    issueIn.issue.ready := Mux(issueIn.issue.bits.uop.ctrl.fuType === FuType.alu, issueAlu.issue.ready, issueJmp.issue.ready)
    private val issueFuHit = issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_ | _)
    xs_assert(Mux(issueIn.issue.valid, issueFuHit, true.B))
  }
}
