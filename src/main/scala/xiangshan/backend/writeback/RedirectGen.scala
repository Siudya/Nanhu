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
  private def oldestSelect(in0:Valid[RedirectSelectBundle], in1:Valid[RedirectSelectBundle], idxWidth:Int, p:Parameters):Valid[RedirectSelectBundle] = {
    val res = Wire(Valid(new RedirectSelectBundle(idxWidth)(p)))
    res.valid := in0.valid | in1.valid
    res.bits := MuxLookup(Cat(in1.valid, in0.valid), in0.bits, Seq(
      "b10".U -> in1.bits,
      "b11".U -> Mux(in0.bits.robIdx < in1.bits.robIdx, in0.bits, in1.bits)
    ))
    res
  }

  def selectOldestRedirect(in: Seq[Valid[Redirect]], p:Parameters): (Valid[Redirect], UInt) = {
    val idxWidth = in.length
    val selectInfo = in.zipWithIndex.map({case(r, idx) =>
      val res = Wire(Valid(new RedirectSelectBundle(idxWidth)(p)))
      res.valid := r.valid
      res.bits.robIdx := r.bits.robIdx
      res.bits.idxOH := 1.U << idx
      res
    })
    val op = oldestSelect(_, _, idxWidth, p)
    val selRes = ParallelOperation(selectInfo, op)
    val res = Wire(Valid(new Redirect()(p)))
    res.valid := selRes.valid
    res.bits := Mux1H(selRes.bits.idxOH, in.map(_.bits))
    (res, selRes.bits.idxOH)
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
  private val (redirectSel, redirectIdxOH) = selectOldestRedirect(allRedirect, p)
  private val redirectValid = redirectSel.valid && !redirectSel.bits.robIdx.needFlush(io.redirectIn)
  private val exuOutSel = Mux1H(redirectIdxOH, allWb)

  private var addrIdx = 0
  private val isJmp = redirectIdxOH(jmpRedirectNum + addrIdx - 1, addrIdx).orR
  addrIdx = addrIdx + jmpRedirectNum
  private val isAlu = redirectIdxOH(aluRedirectNum + addrIdx - 1, addrIdx).orR
  addrIdx = addrIdx + aluRedirectNum
  private val isMem = redirectIdxOH(memRedirectNum + addrIdx - 1, addrIdx).orR
  addrIdx = addrIdx + memRedirectNum

  io.pcReadAddr(0) := redirectSel.bits.ftqIdx.value
  private val s1_isJmpReg = RegEnable(isJmp, redirectValid)
  private val s1_isMemReg = RegEnable(isMem, redirectValid)
  private val s1_pcReadReg = RegEnable(io.pcReadData(0).getPc(redirectSel.bits.ftqOffset), redirectValid)
  private val s1_jmpTargetReg = RegEnable(io.jmpWbIn.head.bits.redirect.cfiUpdate.target, redirectValid)
  private val s1_imm12Reg = RegEnable(exuOutSel.bits.uop.ctrl.imm(11, 0), redirectValid)
  private val s1_pdReg = RegEnable(exuOutSel.bits.uop.cf.pd, redirectValid)
  private val s1_robIdxReg = RegEnable(redirectSel.bits.robIdx, redirectValid)
  private val s1_redirectBitsReg = RegEnable(redirectSel.bits, redirectValid)
  private val s1_redirectValidReg = RegNext(redirectValid, false.B)

  private val branchTarget = s1_pcReadReg + SignExt(ImmUnion.B.toImm32(s1_imm12Reg), XLEN)
  private val snpc = s1_pcReadReg + Mux(s1_pdReg.isRVC, 2.U, 4.U)
  private val redirectTarget = WireInit(snpc)
  when(s1_isMemReg){
    redirectTarget := s1_pcReadReg
  }.elsewhen(s1_redirectBitsReg.isException || s1_redirectBitsReg.isXRet){
    redirectTarget := s1_jmpTargetReg
  }.elsewhen(s1_redirectBitsReg.cfiUpdate.taken){
    redirectTarget := Mux(s1_isJmpReg, s1_jmpTargetReg, branchTarget)
  }
  io.redirectOut.valid := s1_redirectValidReg && !s1_robIdxReg.needFlush(io.redirectIn)
  io.redirectOut.bits := s1_redirectBitsReg
  io.redirectOut.bits.cfiUpdate.pc := s1_pcReadReg
  io.redirectOut.bits.cfiUpdate.pd := s1_pdReg
  io.redirectOut.bits.cfiUpdate.target := redirectTarget


  // get pc from PcMem
  // valid only if redirect is caused by load violation
  // store_pc is used to update store set
  io.pcReadAddr(1) := s1_redirectBitsReg.stFtqIdx.value
  private val shouldUpdateMdp = s1_isMemReg && s1_redirectValidReg && s1_redirectBitsReg.isLoadStore
  private val storePc = RegEnable(io.pcReadData(1).getPc(s1_redirectBitsReg.stFtqOffset), shouldUpdateMdp)

  // update load violation predictor if load violation redirect triggered
  io.memPredUpdate.valid := RegNext(shouldUpdateMdp, init = false.B)
  // update wait table
  io.memPredUpdate.bits.waddr := RegEnable(XORFold(s1_pcReadReg(VAddrBits - 1, 1), MemPredPCWidth), shouldUpdateMdp)
  io.memPredUpdate.bits.wdata := true.B
  // update store set
  io.memPredUpdate.bits.ldpc := RegEnable(XORFold(s1_pcReadReg(VAddrBits - 1, 1), MemPredPCWidth), shouldUpdateMdp)
  // store pc is ready 1 cycle after s1_isReplay is judged
  io.memPredUpdate.bits.stpc := XORFold(storePc(VAddrBits - 1, 1), MemPredPCWidth)
}
