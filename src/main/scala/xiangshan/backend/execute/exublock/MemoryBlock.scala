package xiangshan.backend.execute.exublock

import chipsalliance.rocketchip.config.Parameters
import xiangshan.backend.execute.exu.{ExuConfig, ExuInputNode, ExuOutputNode, ExuType}
import xiangshan.backend.execute.exucx.{ExuComplexIssueNode, ExuComplexWritebackNode}
import xiangshan.backend.execute.fu.FuConfigs
import chisel3._
import chisel3.util._
import xiangshan._

class MemoryBlock(implicit p:Parameters) extends BasicExuBlock{
  private val lduParams = Seq.tabulate(loadNum)(idx => {
    ExuConfig(
      name = "LduExu",
      id = idx,
      complexName = "LduComplex",
      fuConfigs = Seq(FuConfigs.lduCfg),
      exuType = ExuType.ldu
    )
  })
  private val staParams = Seq.tabulate(storeNum)(idx => {
    ExuConfig(
      name = "StaExu",
      id = idx,
      complexName = "StaComplex",
      fuConfigs = Seq(FuConfigs.staCfg),
      exuType = ExuType.sta
    )
  })
  private val stdParams = Seq.tabulate(storeNum)(idx => {
    ExuConfig(
      name = "StdExu",
      id = idx,
      complexName = "StdComplex",
      fuConfigs = Seq(FuConfigs.stdCfg),
      exuType = ExuType.std
    )
  })
  protected[exublock] val lduIssueNodes = lduParams.zipWithIndex.map(new MemoryBlockIssueNode(_))
  protected[exublock] val lduWritebackNodes = lduParams.map(new ExuOutputNode(_))
  protected[exublock] val staIssueNodes = staParams.zipWithIndex.map(new MemoryBlockIssueNode(_))
  protected[exublock] val staWritebackNodes = staParams.map(new ExuOutputNode(_))
  protected[exublock] val stdIssueNodes = stdParams.zipWithIndex.map(new MemoryBlockIssueNode(_))
  protected[exublock] val stdWritebackNodes = stdParams.map(new ExuOutputNode(_))
  private val allIssueNodes = lduIssueNodes ++ staIssueNodes ++ stdIssueNodes
  private val allWritebackNodes = lduWritebackNodes ++ staWritebackNodes ++ stdWritebackNodes

  allIssueNodes.foreach(inode => inode :*= issueNode)
  allWritebackNodes.foreach(onode => writebackNode :=* onode)

  lazy val module = new MemoryBlockImpl(this)
}

class MemoryBlockImpl(outer:MemoryBlock) extends BasicExuBlockImp(outer){

  private val lduIssues = outer.lduIssueNodes.map(iss => {
    require(iss.in.length == 1)
    dontTouch(iss.in.head._1)
    iss.in.head
  })
  private val staIssues = outer.staIssueNodes.map(iss => {
    require(iss.in.length == 1)
    dontTouch(iss.in.head._1)
    iss.in.head
  })
  private val stdIssues = outer.stdIssueNodes.map(iss => {
    require(iss.in.length == 1)
    dontTouch(iss.in.head._1)
    iss.in.head
  })
  private val lduWritebacks = outer.lduWritebackNodes.map(wb => {
    require(wb.out.length == 1)
    dontTouch(wb.out.head._1)
    wb.out.head
  })
  private val staWritebacks = outer.staWritebackNodes.map(wb => {
    require(wb.out.length == 1)
    dontTouch(wb.out.head._1)
    wb.out.head
  })
  private val stdWritebacks = outer.stdWritebackNodes.map(wb => {
    require(wb.out.length == 1)
    dontTouch(wb.out.head._1)
    wb.out.head
  })
  val io = IO(new Bundle{
    val earlyWakeUpCancel = Output(Vec(lduIssues.length, Bool()))
    val issueToMou = Flipped(Decoupled(new ExuInput))
    val writebackFromMou = Decoupled(new ExuOutput)
  })
  io.issueToMou.ready := true.B
  io.writebackFromMou.valid := false.B
  io.writebackFromMou.bits := DontCare
  io.earlyWakeUpCancel := DontCare
  dontTouch(io)
}