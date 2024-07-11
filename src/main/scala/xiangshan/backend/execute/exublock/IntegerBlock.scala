/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/
/***************************************************************************************
 * Author: Liang Sen
 * E-mail: liangsen20z@ict.ac.cn
 * Date: 2023-06-19
 ****************************************************************************************/
package xiangshan.backend.execute.exublock

import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.execute.exucx.{AluDivComplex, AluBruMiscComplex, AluMulComplex, JmpComplex, AluMulDivStdComplex}
import freechips.rocketchip.diplomacy.LazyModule
import chisel3._
import chisel3.util._
import xiangshan.{ExuInput, ExuOutput, XSCoreParamsKey}
import xiangshan.backend.execute.exu.FenceIO
import xiangshan.backend.execute.fu.csr.CSRFileIO
import xiangshan.ExceptionNO.fdiUJumpFault
import xiangshan.backend.execute.fu.csr._
import xiangshan.backend.execute.fu.fence._
import xs.utils._
import xiangshan.Redirect
import xiangshan.FuType

class IntegerBlock(implicit p:Parameters) extends BasicExuBlock {
  println("IntegerBlock's structure:")
  println("jmpNum           = " + jmpNum)
  println("aluMulDivStdNum  = " + aluMulDivStdNum)
  println("aluBruMiscNum    = " + aluBruMiscNum)

  val jmps          = Seq.tabulate(jmpNum)          (idx => LazyModule(new JmpComplex(idx, 0)))
  val aluMulDivStds = Seq.tabulate(aluMulDivStdNum) (idx => LazyModule(new AluMulDivStdComplex(idx, 0)))
  val aluBruMiscs   = Seq.tabulate(aluBruMiscNum)   (idx => LazyModule(new AluBruMiscComplex(idx, 0)))
  
  val intComplexes = aluMulDivStds ++ aluBruMiscs ++ jmps
  intComplexes.foreach(exucx => {
    exucx.issueNode :*= issueNode
    writebackNode   :=* exucx.writebackNode
  })

  lazy val module = new IntegerBlockImp(this)
}
class IntegerBlockImp(outer: IntegerBlock) extends BasicExuBlockImp(outer) {
  val io = IO(new Bundle {
    val fenceio           = new FenceIO
    val csrio             = new CSRFileIO
    val issueToMou        = Decoupled(new ExuInput)
    val writebackFromMou  = Flipped(Decoupled(new ExuOutput))
    val prefetchI         = Output(Valid(UInt(p(XSCoreParamsKey).XLEN.W)))
  })

  val fence = Module(new Fence)
  val csr   = Module(new CSR)

  outer.intComplexes.foreach(_.module.redirectIn := Pipe(redirectIn))
  fence.io.redirectIn := Pipe(redirectIn)
  csr.io.redirectIn   := Pipe(redirectIn)

  fence.io.out.ready := true.B
  csr.io.out.ready := true.B

  (outer.aluMulDivStds ++ outer.aluBruMiscs ++ outer.jmps)
    .foreach(cplx => cplx.module.bypassIn.zip(outer.aluMulDivStds.map(_.module.io.bypassOut))
      .foreach({ case (a, b) => a := b }))

  val miscNum = outer.aluBruMiscs.length
  println("intBlock has " + miscNum + " misc")

  io.fenceio.sfence         := fence.sfence
  io.fenceio.fencei         <> fence.fencei
  io.fenceio.sbuffer        <> fence.toSbuffer
  fence.toSbuffer.sbIsEmpty := io.fenceio.sbuffer.sbIsEmpty
  fence.disableSfence       := csr.csrio.disableSfence
  fence.priviledgeMode      := csr.csrio.priviledgeMode

  outer.aluBruMiscs.foreach {
    case md => {
      md.module.io.issueToCSR.ready := csr.io.in.ready
      md.module.io.issueToFence.ready := fence.io.in.ready
      md.module.io.issueToMou.ready := io.issueToMou.ready
      md.module.redirectIn := redirectIn
    }
  }


  val fenceInHitVec = outer.aluBruMiscs.map(_.module.io.issueToFence.valid)
  val fenceInVec    = outer.aluBruMiscs.map(_.module.io.issueToFence.bits)
  fence.io.in.valid := fenceInHitVec.reduce(_||_)
  fence.io.in.bits  := Mux1H(fenceInHitVec, fenceInVec)

  val fenceInVecReg = Reg(Vec(miscNum, Bool()))
  when(fence.io.in.valid) {
    fenceInVecReg := VecInit(fenceInHitVec)
  }

  outer.aluBruMiscs.map(_.module.io.writebackFromFence).zip(outer.aluBruMiscs.map(_.module.io.redirectFromFence)).zip(fenceInVecReg).foreach {
    case((wb, r), en) => {
      wb.valid  := en && fence.io.out.valid
      wb.bits   := fence.io.out.bits
      r.valid   := en && fence.redirectOutValid
      r.bits    := fence.redirectOut
    }
  }

  val csrInHitVec = outer.aluBruMiscs.map(_.module.io.issueToCSR.valid)
  val csrInVec    = outer.aluBruMiscs.map(_.module.io.issueToCSR.bits)
  csr.io.in.valid := csrInHitVec.reduce(_||_)
  csr.io.in.bits  := Mux1H(csrInHitVec, csrInVec)

  outer.aluBruMiscs.map(_.module.io.writebackFromCSR).zip(outer.aluBruMiscs.map(_.module.io.redirectFromCSR)).zip(outer.aluBruMiscs.map(_.module.io.issueToCSR.valid)).foreach {
    case ((wb, r), en) => {
      wb.valid  := csr.io.out.valid && en
      wb.bits   := csr.io.out.bits
      r.valid   := en && csr.redirectOutValid
      r.bits    := csr.redirectOut
    }
  }

  val mouInHitVec = outer.aluBruMiscs.map(_.module.io.issueToMou.valid)
  val mouInVec    = outer.aluBruMiscs.map(_.module.io.issueToMou.bits)
  io.issueToMou.valid := mouInHitVec.reduce(_||_)
  io.issueToMou.bits  := Mux1H(mouInHitVec, mouInVec)

  val mouInVecReg = Reg(Vec(miscNum, Bool()))
  when(io.issueToMou.valid) {
    mouInVecReg := VecInit(fenceInHitVec)
  }

  val mouReadyVec = outer.aluBruMiscs.map(_.module.io.writebackFromMou.ready)
  io.writebackFromMou.ready := Mux1H(mouInHitVec, mouReadyVec)
  outer.aluBruMiscs.map(_.module.io.writebackFromMou).zip(mouInVecReg).foreach {
    case(wb, en) => {
      wb.valid  := en && io.writebackFromMou.valid
      wb.bits   := io.writebackFromMou.bits
    }
  }

  outer.aluBruMiscs.foreach(misc => misc.module.io.csrIsPerfCnt := csr.csrio.isPerfCnt)

  csr.csrio <> io.csrio
  csr.csrio.vcsr.robWb.vxsat            := io.csrio.vcsr.robWb.vxsat
  csr.csrio.vcsr.robWb.vstart           := io.csrio.vcsr.robWb.vstart
  io.csrio.vcsr.vtype.vtypeRead.readEn  := RegNext(csr.csrio.vcsr.vtype.vtypeRead.readEn, false.B)
  io.csrio.vcsr.vtype.vlRead.readEn     := RegNext(csr.csrio.vcsr.vtype.vlRead.readEn, false.B)
  csr.csrio.vcsr.vtype.vlUpdate         := Pipe(io.csrio.vcsr.vtype.vlUpdate)
  io.csrio.tlb                          := DelayN(csr.csrio.tlb, 2)
  io.csrio.customCtrl                   := DelayN(csr.csrio.customCtrl, 2)
  csr.csrio.exception                   := Pipe(io.csrio.exception)

  val jmp_module = outer.jmps.head.module
  val fdiUJumpExcpVAddr = RegEnable(jmp_module.io.fdicallJumpExcpIO.target, jmp_module.io.fdicallJumpExcpIO.isJumpExcp)
  // FDICALL.JR will write FDIReturnPC CSR
  private val fdicallValid     = RegNext(jmp_module.io.fdicallJumpExcpIO.isFDICall, false.B)
  private val fdicallTargetReg = RegEnable(jmp_module.io.fdicallJumpExcpIO.target, jmp_module.io.fdicallJumpExcpIO.isFDICall)
  when (fdicallValid) {
    csr.io.in.valid                   := true.B
    csr.io.in.bits.src(0)             := fdicallTargetReg
    csr.io.in.bits.uop.ctrl.rfWen     := false.B
    csr.io.in.bits.uop.ctrl.imm       := csr.Fdireturnpc.U
    csr.io.in.bits.uop.ctrl.fuOpType  := CSROpType.wrt
  }

  csr.csrio.customCtrl.distribute_csr <> jmp_module.io.fdicallDistributedCSR
  csr.csrio.memExceptionVAddr :=
    Mux(csr.csrio.exception.bits.uop.cf.exceptionVec(fdiUJumpFault),
      fdiUJumpExcpVAddr, io.csrio.memExceptionVAddr)

  outer.aluMulDivStds.foreach(_.module.io.csr_frm := csr.csrio.fpu.frm)
  io.prefetchI := outer.jmps.head.module.io.prefetchI
}
