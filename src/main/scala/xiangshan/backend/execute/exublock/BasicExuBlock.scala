package xiangshan.backend.execute.exublock

import chipsalliance.rocketchip.config.Parameters
import chisel3.util.Valid
import chisel3.{Input, Vec}
import xiangshan.{ExuOutput, Redirect, XSCoreParamsKey}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

abstract class BasicExuBlock(implicit p:Parameters) extends LazyModule{
  val issueNode = new ExuBlockIssueNode
  val writebackNode = new ExuBlockWritebackNode
  protected val mulNum:Int = p(XSCoreParamsKey).exuParameters.AluMulCnt
  protected val divNum:Int = p(XSCoreParamsKey).exuParameters.AluDivCnt
  protected val jmpNum:Int = p(XSCoreParamsKey).exuParameters.AluJmpCnt
  protected val fmaNum:Int = p(XSCoreParamsKey).exuParameters.FmaCnt
  protected val fmaMiscNum:Int = p(XSCoreParamsKey).exuParameters.FmaMiscCnt
  protected val fmaDivNum:Int = p(XSCoreParamsKey).exuParameters.FmaDivCnt
  protected val loadNum:Int = p(XSCoreParamsKey).exuParameters.LduCnt
  protected val storeNum:Int = p(XSCoreParamsKey).exuParameters.StuCnt
  override def module:BasicExuBlockImp
}

class BasicExuBlockImp(outer:BasicExuBlock) extends LazyModuleImp(outer){
  val redirectIn = IO(Input(Valid(new Redirect)))
  outer.writebackNode.in.zip(outer.writebackNode.out).foreach({
    case (source, sink) =>
      sink._1 := source._1
  })
  outer.issueNode.in.zip(outer.issueNode.out).foreach({
    case (source, sink) =>
      sink._1 <> source._1
  })
}