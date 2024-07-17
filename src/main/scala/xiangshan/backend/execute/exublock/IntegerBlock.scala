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
import xiangshan.backend.execute.exucx._
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
  // val jmps          = Seq.tabulate(jmpNum)          (idx => LazyModule(new JmpComplex(idx, 0)))
  // val aluMulDivStds = Seq.tabulate(aluMulDivStdNum) (idx => LazyModule(new AluMulDivStdComplex(idx, 0)))
  // val aluBruMiscs   = Seq.tabulate(aluBruMiscNum)   (idx => LazyModule(new AluBruMiscComplex(idx, 0)))
  // val aluMulJmps    = Seq.tabulate(aluMulJmpNum)    (idx => LazyModule(new AluMulJmpComplex(idx, 0)))
  // val mulDivs       = Seq.tabulate(mulDivNum)       (idx => LazyModule(new MulDivComplex(idx, 0)))
  val aluBruJmps = Seq.tabulate(aluBruJmpNum)          (idx => LazyModule(new AluBruJmpComplex(idx, 0)))
  val aluMuls = Seq.tabulate(aluMulNum)          (idx => LazyModule(new AluMulComplex(idx, 0)))
  val divMisc = Seq.tabulate(divMiscNum)          (idx => LazyModule(new DivMiscComplex(idx, 0)))
  
  val intComplexes = aluBruJmps ++ aluMuls ++ divMisc
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

  // (outer.aluBruMiscs ++ outer.aluMulJmps ++ outer.mulDivs)
  //   .foreach(cplx => cplx.module.bypassIn.zip(outer.aluMulJmps.map(_.module.io.bypassOut))
  //     .foreach({ case (a, b) => a := b }))

  val miscNum = outer.divMisc.length
  println("intBlock has " + miscNum + " misc")

  io.fenceio.sfence         := fence.sfence
  io.fenceio.fencei         <> fence.fencei
  io.fenceio.sbuffer        <> fence.toSbuffer
  fence.toSbuffer.sbIsEmpty := io.fenceio.sbuffer.sbIsEmpty
  fence.disableSfence       := csr.csrio.disableSfence
  fence.priviledgeMode      := csr.csrio.priviledgeMode

  outer.divMisc.foreach {
    case md => {
      md.module.io.issueToCSR.ready := csr.io.in.ready
      md.module.io.issueToFence.ready := fence.io.in.ready
      md.module.io.issueToMou.ready := io.issueToMou.ready
      md.module.redirectIn := redirectIn
    }
  }


  val fenceInHitVec = outer.divMisc.map(_.module.io.issueToFence.valid)
  val fenceInVec    = outer.divMisc.map(_.module.io.issueToFence.bits)
  fence.io.in.valid := fenceInHitVec.reduce(_||_)
  fence.io.in.bits  := Mux1H(fenceInHitVec, fenceInVec)

  val fenceInVecReg = Reg(Vec(miscNum, Bool()))
  when(fence.io.in.valid) {
    fenceInVecReg := VecInit(fenceInHitVec)
  }

  outer.divMisc.map(_.module.io.writebackFromFence).zip(outer.divMisc.map(_.module.io.redirectFromFence)).zip(fenceInVecReg).foreach {
    case((wb, r), en) => {
      wb.valid  := en && fence.io.out.valid
      wb.bits   := fence.io.out.bits
      r.valid   := fence.redirectOutValid
      r.bits    := fence.redirectOut
    }
  }

  val csrInHitVec = outer.divMisc.map(_.module.io.issueToCSR.valid)
  val csrInVec    = outer.divMisc.map(_.module.io.issueToCSR.bits)
  csr.io.in.valid := csrInHitVec.reduce(_||_)
  csr.io.in.bits  := Mux1H(csrInHitVec, csrInVec)

  outer.divMisc.map(_.module.io.writebackFromCSR).zip(outer.divMisc.map(_.module.io.redirectFromCSR)).zip(outer.divMisc.map(_.module.io.issueToCSR.valid)).foreach {
    case ((wb, r), en) => {
      wb.valid  := csr.io.out.valid && en
      wb.bits   := csr.io.out.bits
      r.valid   := csr.redirectOutValid
      r.bits    := csr.redirectOut
    }
  }

  val mouInHitVec = outer.divMisc.map(_.module.io.issueToMou.valid)
  val mouInVec    = outer.divMisc.map(_.module.io.issueToMou.bits)
  io.issueToMou.valid := mouInHitVec.reduce(_||_)
  io.issueToMou.bits  := Mux1H(mouInHitVec, mouInVec)

  val mouInVecReg = Reg(Vec(miscNum, Bool()))
  when(io.issueToMou.valid) {
    mouInVecReg := VecInit(fenceInHitVec)
  }

  val mouReadyVec = outer.divMisc.map(_.module.io.writebackFromMou.ready)
  io.writebackFromMou.ready := Mux1H(mouInHitVec, mouReadyVec)
  outer.divMisc.map(_.module.io.writebackFromMou).zip(mouInVecReg).foreach {
    case(wb, en) => {
      wb.valid  := en && io.writebackFromMou.valid
      wb.bits   := io.writebackFromMou.bits
    }
  }

  outer.divMisc.foreach(misc => misc.module.io.csrIsPerfCnt := csr.csrio.isPerfCnt)

  csr.csrio <> io.csrio
  csr.csrio.vcsr.robWb.vxsat            := io.csrio.vcsr.robWb.vxsat
  csr.csrio.vcsr.robWb.vstart           := io.csrio.vcsr.robWb.vstart
  io.csrio.vcsr.vtype.vtypeRead.readEn  := RegNext(csr.csrio.vcsr.vtype.vtypeRead.readEn, false.B)
  io.csrio.vcsr.vtype.vlRead.readEn     := RegNext(csr.csrio.vcsr.vtype.vlRead.readEn, false.B)
  csr.csrio.vcsr.vtype.vlUpdate         := Pipe(io.csrio.vcsr.vtype.vlUpdate)
  io.csrio.tlb                          := DelayN(csr.csrio.tlb, 2)
  io.csrio.customCtrl                   := DelayN(csr.csrio.customCtrl, 2)
  csr.csrio.exception                   := Pipe(io.csrio.exception)

  private val jmps = outer.aluBruJmps.map(_.module)
  private val fdiUJumpExcpVec = WireInit(VecInit(jmps.map(_.io.fdicallJumpExcpIO.isJumpExcp)))
  private val fdiUJumpExcpVAddrVec = WireInit(VecInit(jmps.map(_.io.fdicallJumpExcpIO.target)))
  private val fdiUJumpExcpVAddr = RegEnable(Mux1H(fdiUJumpExcpVec, fdiUJumpExcpVAddrVec), fdiUJumpExcpVec.reduce(_||_))
  // FDICALL.JR will write FDIReturnPC CSR
  private val fdicallValidVec  = WireInit(VecInit(jmps.map(_.io.fdicallJumpExcpIO.isFDICall)))
  private val fdicallValid     = RegNext(fdicallValidVec.reduce(_||_), false.B)
  private val fdicallTargetReg = RegEnable(Mux1H(fdicallValidVec, fdiUJumpExcpVAddrVec), fdicallValidVec.reduce(_||_))
  when (fdicallValid) {
    csr.io.in.valid                   := true.B
    csr.io.in.bits.src(0)             := fdicallTargetReg
    csr.io.in.bits.uop.ctrl.rfWen     := false.B
    csr.io.in.bits.uop.ctrl.imm       := csr.Fdireturnpc.U
    csr.io.in.bits.uop.ctrl.fuOpType  := CSROpType.wrt
  }

  jmps.map(_.io.fdicallDistributedCSR).foreach(_ := csr.csrio.customCtrl.distribute_csr)

  csr.csrio.memExceptionVAddr :=
    Mux(csr.csrio.exception.bits.uop.cf.exceptionVec(fdiUJumpFault),
      fdiUJumpExcpVAddr, io.csrio.memExceptionVAddr)

  outer.aluMuls.foreach(_.module.io.csr_frm := csr.csrio.fpu.frm)

  val prefetchIQ = Seq.tabulate(outer.aluBruJmps.length)(i => Module(new Queue(UInt(p(XSCoreParamsKey).XLEN.W), 2)))
  prefetchIQ.zip(outer.aluBruJmps).foreach {
    case (q, jmp) => {
      q.io.enq.valid := jmp.module.io.prefetchI.valid
      q.io.enq.bits := jmp.module.io.prefetchI.bits
    }
  }

  val prefetchIQArb = Module(new RRArbiter(UInt(p(XSCoreParamsKey).XLEN.W), outer.aluBruJmps.length))
  prefetchIQArb.io.in.zip(prefetchIQ.map(_.io.deq)).foreach {
    case (arb, deq) => {
      arb.bits := deq.bits
      arb.valid := deq.valid
      deq.ready := arb.ready
    }
  }
  io.prefetchI.bits := prefetchIQArb.io.out.bits
  io.prefetchI.valid := prefetchIQArb.io.out.valid
  prefetchIQArb.io.out.ready := true.B
}
