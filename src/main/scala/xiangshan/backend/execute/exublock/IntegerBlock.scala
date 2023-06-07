package xiangshan.backend.execute.exublock

import chipsalliance.rocketchip.config.Parameters
import xiangshan.backend.execute.exucx.{AluDivComplex, AluJmpComplex, AluMulComplex}
import freechips.rocketchip.diplomacy.LazyModule
import chisel3._
import chisel3.util._
import xiangshan.{ExuInput, ExuOutput}
import xiangshan.backend.execute.exu.FenceIO
import xiangshan.backend.execute.fu.csr.CSRFileIO

class IntegerBlock(implicit p:Parameters) extends BasicExuBlock {
  require(jmpNum == 1)
  private val aluMuls = Seq.tabulate(mulNum)(idx => LazyModule(new AluMulComplex(idx, mulNum - 1)))
  private val aluDivs = Seq.tabulate(divNum)(idx => LazyModule(new AluDivComplex(idx, mulNum)))
  private val aluJmps = Seq.tabulate(jmpNum)(idx => LazyModule(new AluJmpComplex(idx, mulNum)))
  private val intComplexes = aluMuls ++ aluDivs ++ aluJmps
  intComplexes.foreach(exucx => {
    exucx.issueNode :*= issueNode
    writebackNode :=* exucx.writebackNode
  })

  lazy val module = new BasicExuBlockImp(this){
    val io = IO(new Bundle {
      val fenceio = new FenceIO
      val csrio = new CSRFileIO
      val issueToMou = Decoupled(new ExuInput)
      val writebackFromMou = Flipped(Decoupled(new ExuOutput))
    })
    intComplexes.foreach(_.module.redirectIn := Pipe(redirectIn))

    (aluJmps ++ aluDivs).foreach(cplx => cplx.module.bypassIn.zip(aluMuls.map(_.module.io.bypassOut)).foreach({ case (a, b) => a := b }))

    aluMuls.map(_.module.bypassIn).zipWithIndex.foreach({ case (bin, idx) =>
      val sources = aluMuls.zipWithIndex.filterNot(_._2 == idx).map(_._1.module.io.bypassOut)
      bin.zip(sources).foreach({ case (a, b) => a := b })
    })

    aluJmps.head.module.io.fenceio <> io.fenceio
    aluJmps.head.module.io.csrio <> io.csrio
    aluJmps.head.module.io.issueToMou <> io.issueToMou
    aluJmps.head.module.io.writebackFromMou <> io.writebackFromMou
  }
}
