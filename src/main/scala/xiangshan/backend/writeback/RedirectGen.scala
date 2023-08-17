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
import chipsalliance.rocketchip.config.Parameters
import xiangshan._
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.issue.SelectPolicy
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.Ftq_RF_Components
import xs.utils.{ParallelOperation, SignExt, XORFold}

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
    val selector = Module(new SelectPolicy(in.length, true, false)(p))
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

class RedirectGen(jmpRedirectNum:Int, aluRedirectNum:Int, memRedirectNum:Int)(implicit p: Parameters) extends XSModule{
  require(jmpRedirectNum == 1)
  val io = IO(new Bundle{
    val jmpWbIn = Input(Vec(jmpRedirectNum, Flipped(ValidIO(new ExuOutput))))
    val aluWbIn = Input(Vec(aluRedirectNum, Flipped(ValidIO(new ExuOutput))))
    val memWbIn = Input(Vec(memRedirectNum, Flipped(ValidIO(new ExuOutput))))
    val pcReadAddr = Output(Vec(2, UInt(log2Ceil(FtqSize).W)))
    val pcReadData = Input(Vec(2, new Ftq_RF_Components))
    val redirectIn = Input(Valid(new Redirect))
    val redirectOut = Output(Valid(new Redirect))
    val memPredUpdate = Output(Valid(new MemPredUpdateReq))
  })

  private val allWb = io.jmpWbIn ++ io.aluWbIn ++ io.memWbIn
  private val allRedirect = allWb.map(getRedirect(_, p))
  private val (redirectSel, redirectIdxOH) = selectOldest(allRedirect, p)
  private val redirectValid = redirectSel.valid && !redirectSel.bits.robIdx.needFlush(io.redirectIn)
  private val exuOutSel = Mux1H(redirectIdxOH, allWb)

  private val s1_redirectValidReg = RegNext(redirectValid, false.B)
  private val s1_redirectBitsReg = RegEnable(redirectSel.bits, redirectValid)
  private val s1_redirectIdxOHReg = RegEnable(redirectIdxOH, redirectValid)
  private val s1_jmpTargetReg = RegEnable(io.jmpWbIn.head.bits.redirect.cfiUpdate.target, redirectValid)
  private val s1_uopReg = RegEnable(exuOutSel.bits.uop, redirectValid)

  private val s1_redirectValid = s1_redirectValidReg && !s1_redirectBitsReg.robIdx.needFlush(io.redirectIn)

  private var addrIdx = 0
  private val isJmp = s1_redirectIdxOHReg(jmpRedirectNum + addrIdx - 1, addrIdx).orR
  addrIdx = addrIdx + jmpRedirectNum
  private val isAlu = s1_redirectIdxOHReg(aluRedirectNum + addrIdx - 1, addrIdx).orR
  addrIdx = addrIdx + aluRedirectNum
  private val isMem = s1_redirectIdxOHReg(memRedirectNum + addrIdx - 1, addrIdx).orR
  addrIdx = addrIdx + memRedirectNum

  io.pcReadAddr(0) := s1_redirectBitsReg.ftqIdx.value

  private val s2_isJmpReg = RegEnable(isJmp, s1_redirectValid)
  private val s2_isMemReg = RegEnable(isMem, s1_redirectValid)
  private val s2_pcReadReg = RegEnable(io.pcReadData(0).getPc(s1_redirectBitsReg.ftqOffset), s1_redirectValid)
  private val s2_jmpTargetReg = RegEnable(s1_jmpTargetReg, s1_redirectValid)
  private val s2_imm12Reg = RegEnable(s1_uopReg.ctrl.imm(11, 0), s1_redirectValid)
  private val s2_pdReg = RegEnable(s1_uopReg.cf.pd, s1_redirectValid)
  private val s2_robIdxReg = RegEnable(s1_redirectBitsReg.robIdx, s1_redirectValid)
  private val s2_redirectBitsReg = RegEnable(s1_redirectBitsReg, s1_redirectValid)
  private val s2_redirectValidReg = RegNext(s1_redirectValid, false.B)

  private val branchTarget = s2_pcReadReg + SignExt(ImmUnion.B.toImm32(s2_imm12Reg), XLEN)
  private val snpc = s2_pcReadReg + Mux(s2_pdReg.isRVC, 2.U, 4.U)
  private val redirectTarget = WireInit(snpc)
  when(s2_isMemReg){
    redirectTarget := s2_pcReadReg
  }.elsewhen(s2_redirectBitsReg.isException || s2_redirectBitsReg.isXRet){
    redirectTarget := s2_jmpTargetReg
  }.elsewhen(s2_redirectBitsReg.cfiUpdate.taken){
    redirectTarget := Mux(s2_isJmpReg, s2_jmpTargetReg, branchTarget)
  }
  io.redirectOut.valid := s2_redirectValidReg && !s2_robIdxReg.needFlush(io.redirectIn)
  io.redirectOut.bits := s2_redirectBitsReg
  io.redirectOut.bits.cfiUpdate.pc := s2_pcReadReg
  io.redirectOut.bits.cfiUpdate.pd := s2_pdReg
  io.redirectOut.bits.cfiUpdate.target := redirectTarget


  // get pc from PcMem
  // valid only if redirect is caused by load violation
  // store_pc is used to update store set
  io.pcReadAddr(1) := s2_redirectBitsReg.stFtqIdx.value
  private val shouldUpdateMdp = s2_isMemReg && s2_redirectValidReg && s2_redirectBitsReg.isLoadStore
  private val storePc = RegEnable(io.pcReadData(1).getPc(s2_redirectBitsReg.stFtqOffset), shouldUpdateMdp)

  // update load violation predictor if load violation redirect triggered
  io.memPredUpdate.valid := RegNext(shouldUpdateMdp, init = false.B)
  // update wait table
  io.memPredUpdate.bits.waddr := RegEnable(XORFold(s2_pcReadReg(VAddrBits - 1, 1), MemPredPCWidth), shouldUpdateMdp)
  io.memPredUpdate.bits.wdata := true.B
  // update store set
  io.memPredUpdate.bits.ldpc := RegEnable(XORFold(s2_pcReadReg(VAddrBits - 1, 1), MemPredPCWidth), shouldUpdateMdp)
  // store pc is ready 1 cycle after s2_isReplay is judged
  io.memPredUpdate.bits.stpc := XORFold(storePc(VAddrBits - 1, 1), MemPredPCWidth)
}
