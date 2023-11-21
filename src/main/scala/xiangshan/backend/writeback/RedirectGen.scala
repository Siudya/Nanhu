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
package xiangshan.backend.writeback

import chisel3._
import chisel3.util._
import RedirectGen._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.issue.SelectPolicy
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.Ftq_RF_Components
import xs.utils.{SignExt, XORFold}
import xs.utils.perf.HasPerfLogging

class RedirectSelectBundle(idxWidth:Int)(implicit p: Parameters) extends Bundle{
  val robIdx = new RobPtr
  val idxOH = UInt(idxWidth.W)
}

object RedirectGen{
  private def getRedirect(exuOut: Valid[ExuOutput], p:Parameters): ValidIO[Redirect] = {
    val redirect = Wire(Valid(new Redirect()(p)))
    val ri = exuOut.bits.redirect
    redirect.valid := exuOut.bits.redirectValid && (ri.cfiUpdate.isMisPred || ri.isException || ri.isLoadStore || ri.isLoadLoad || ri.isFlushPipe)
    redirect.bits := exuOut.bits.redirect
    redirect
  }

  private def selectOldest(in: Seq[Valid[Redirect]], p: Parameters): (Valid[Redirect], UInt) = {
    val selector = Module(new SelectPolicy(in.length, true, true)(p))
    selector.io.in.zip(in).foreach({ case (a, b) =>
      a.valid := b.valid
      a.bits := b.bits.robIdx
    })
    val res = Wire(Valid(new Redirect()(p)))
    res.valid := selector.io.out.valid
    res.bits := Mux1H(selector.io.out.bits, in.map(_.bits))
    (res, selector.io.out.bits)
  }
}

class RedirectGen(jmpRedirectNum:Int, aluRedirectNum:Int, memRedirectNum:Int)(implicit p: Parameters) extends XSModule with HasPerfLogging{
  val io = IO(new Bundle{
    val jmpWbIn = Input(Vec(jmpRedirectNum, Flipped(ValidIO(new ExuOutput))))
    val aluWbIn = Input(Vec(aluRedirectNum, Flipped(ValidIO(new ExuOutput))))
    val memWbIn = Input(Vec(memRedirectNum, Flipped(ValidIO(new ExuOutput))))
    val pcReadAddr = Output(Vec(2, UInt(log2Ceil(FtqSize).W)))
    val pcReadData = Input(Vec(2, new Ftq_RF_Components))
    val redirectIn = Input(Valid(new Redirect))
    val redirectOut = Output(Valid(new Redirect))
    val preWalk = Output(Valid(new Redirect))
    val memPredUpdate = Output(Valid(new MemPredUpdateReq))
  })
  private val s1_allWb = Wire(Vec(jmpRedirectNum + aluRedirectNum + memRedirectNum, Valid(new ExuOutput)))
  s1_allWb.zip(io.jmpWbIn ++ io.aluWbIn ++ io.memWbIn).foreach({case(s, wb) =>
    s := DontCare
    val redirectValidCond = wb.bits.redirectValid && !wb.bits.redirect.robIdx.needFlush(io.redirectIn)
    s.valid := RegNext(wb.valid, false.B)
    s.bits.redirectValid := RegNext(redirectValidCond, false.B)
    s.bits.uop := RegEnable(wb.bits.uop, wb.valid)
    s.bits.redirect := RegEnable(wb.bits.redirect, redirectValidCond)
  })
  private val s1_allRedirect = s1_allWb.map(getRedirect(_, p))
  private val (s1_redirectSel, s1_redirectIdxOH) = selectOldest(s1_allRedirect, p)
  private val s1_redirectValid = s1_redirectSel.valid && !s1_redirectSel.bits.robIdx.needFlush(io.redirectIn)
  private val s1_exuOutSel = Mux1H(s1_redirectIdxOH, s1_allWb)
  private val s1_target = Mux1H(s1_redirectIdxOH(jmpRedirectNum - 1, 0), s1_allWb.take(jmpRedirectNum).map(_.bits.redirect.cfiUpdate.target))

  private val s2_redirectValidReg = RegNext(s1_redirectValid, false.B)
  private val s2_redirectBitsReg = RegEnable(s1_redirectSel.bits, s1_redirectValid)
  private val s2_redirectIdxOHReg = RegEnable(s1_redirectIdxOH, s1_redirectValid)
  private val s2_jmpTargetReg = RegEnable(s1_target, s1_redirectValid)
  private val s2_uopReg = RegEnable(s1_exuOutSel.bits.uop, s1_redirectValid)

  private val s2_redirectValid = s2_redirectValidReg && !s2_redirectBitsReg.robIdx.needFlush(io.redirectIn)

  private var addrIdx = 0
  private val isJmp = s2_redirectIdxOHReg(jmpRedirectNum + addrIdx - 1, addrIdx).orR
  addrIdx = addrIdx + jmpRedirectNum
  private val isAlu = s2_redirectIdxOHReg(aluRedirectNum + addrIdx - 1, addrIdx).orR
  addrIdx = addrIdx + aluRedirectNum
  private val isMem = s2_redirectIdxOHReg(memRedirectNum + addrIdx - 1, addrIdx).orR
  addrIdx = addrIdx + memRedirectNum

  io.pcReadAddr(0) := s2_redirectBitsReg.ftqIdx.value

  private val s3_isJmpReg = RegEnable(isJmp, s2_redirectValid)
  private val s3_isMemReg = RegEnable(isMem, s2_redirectValid)
  private val s3_pcReadReg = RegEnable(io.pcReadData(0).getPc(s2_redirectBitsReg.ftqOffset), s2_redirectValid)
  private val s3_jmpTargetReg = RegEnable(s2_jmpTargetReg, s2_redirectValid)
  private val s3_imm12Reg = RegEnable(s2_uopReg.ctrl.imm(11, 0), s2_redirectValid)
  private val s3_pdReg = RegEnable(s2_uopReg.cf.pd, s2_redirectValid)
  private val s3_robIdxReg = RegEnable(s2_redirectBitsReg.robIdx, s2_redirectValid)
  private val s3_redirectBitsReg = RegEnable(s2_redirectBitsReg, s2_redirectValid)
  private val s3_redirectValidReg = RegNext(s2_redirectValid, false.B)

  private val branchTarget = s3_pcReadReg + SignExt(ImmUnion.B.toImm32(s3_imm12Reg), XLEN)
  private val snpc = s3_pcReadReg + Mux(s3_pdReg.isRVC, 2.U, 4.U)
  private val redirectTarget = WireInit(snpc)
  when(s3_isMemReg){
    redirectTarget := s3_pcReadReg
  }.elsewhen(s3_redirectBitsReg.isException || s3_redirectBitsReg.isXRet){
    redirectTarget := s3_jmpTargetReg
  }.elsewhen(s3_redirectBitsReg.cfiUpdate.taken){
    redirectTarget := Mux(s3_isJmpReg, s3_jmpTargetReg, branchTarget)
  }
  io.redirectOut.valid := s3_redirectValidReg && !s3_robIdxReg.needFlush(io.redirectIn)
  io.redirectOut.bits := s3_redirectBitsReg
  io.redirectOut.bits.cfiUpdate.pc := s3_pcReadReg
  io.redirectOut.bits.cfiUpdate.pd := s3_pdReg
  io.redirectOut.bits.cfiUpdate.target := redirectTarget
  io.redirectOut.bits.isPreWalk := false.B

  io.preWalk.bits := DontCare
  io.preWalk.valid := s1_redirectSel.valid & !s2_redirectValidReg & !s3_redirectValidReg
  io.preWalk.bits.robIdx := PriorityMux(s1_allRedirect.map(_.valid), s1_allRedirect.map(_.bits.robIdx))
  io.preWalk.bits.level := RedirectLevel.flushAfter
  io.preWalk.bits.isException := false.B
  io.preWalk.bits.isLoadStore := false.B
  io.preWalk.bits.isLoadLoad := false.B
  io.preWalk.bits.isFlushPipe := false.B
  io.preWalk.bits.cfiUpdate.isMisPred := false.B
  io.preWalk.bits.isPreWalk := true.B

  // get pc from PcMem
  // valid only if redirect is caused by load violation
  // store_pc is used to update store set
  io.pcReadAddr(1) := s3_redirectBitsReg.stFtqIdx.value
  private val shouldUpdateMdp = s3_isMemReg && s3_redirectValidReg && s3_redirectBitsReg.isLoadStore
  private val storePc = RegEnable(io.pcReadData(1).getPc(s3_redirectBitsReg.stFtqOffset), shouldUpdateMdp)

  // update load violation predictor if load violation redirect triggered
  io.memPredUpdate.valid := RegNext(shouldUpdateMdp, init = false.B)
  // update wait table
  io.memPredUpdate.bits.waddr := RegEnable(XORFold(s3_pcReadReg(VAddrBits - 1, 1), MemPredPCWidth), shouldUpdateMdp)
  io.memPredUpdate.bits.wdata := true.B
  // update store set
  io.memPredUpdate.bits.ldpc := RegEnable(XORFold(s3_pcReadReg(VAddrBits - 1, 1), MemPredPCWidth), shouldUpdateMdp)
  // store pc is ready 1 cycle after s3_isReplay is judged
  io.memPredUpdate.bits.stpc := XORFold(storePc(VAddrBits - 1, 1), MemPredPCWidth)

  XSPerfAccumulate("total_redirect_num", io.redirectOut.valid)
  XSPerfAccumulate("miss_pred_redirect_num", io.redirectOut.valid && io.redirectOut.bits.cfiUpdate.isMisPred)
  XSPerfAccumulate("bad_taken_redirect_num", io.redirectOut.valid && io.redirectOut.bits.cfiUpdate.isMisPred && io.redirectOut.bits.cfiUpdate.taken)
  XSPerfAccumulate("bad_not_taken_redirect_num", io.redirectOut.valid && io.redirectOut.bits.cfiUpdate.isMisPred && !io.redirectOut.bits.cfiUpdate.taken)
  XSPerfAccumulate("load_store_redirect_num", io.redirectOut.valid && io.redirectOut.bits.isLoadStore)
  XSPerfAccumulate("load_load_redirect_num", io.redirectOut.valid && io.redirectOut.bits.isLoadLoad)
  XSPerfAccumulate("exception_redirect_num", io.redirectOut.valid && io.redirectOut.bits.isException)
  XSPerfAccumulate("flush_redirect_num", io.redirectOut.valid && io.redirectOut.bits.isFlushPipe)
}
