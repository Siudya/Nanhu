/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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


package xiangshan.backend.execute.fu.bru

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.fu.FUWithRedirect
import xiangshan.{FuOpType, RedirectLevel, XSModule}
import xs.utils.{LookupTree, SignExt, ZeroExt}

class BranchUnit(implicit p: Parameters) extends FUWithRedirect {
  private val uop = io.in.bits.uop
  private val src = io.in.bits.src.take(2)
  private val func = uop.ctrl.fuOpType

  private val sub = (src(0) +& (~src(1)).asUInt) + 1.U
  
  private val beqTaken  = src(0) === src(1)
  private val bltuTaken = !sub(XLEN)
  private val bltTaken  = src(0)(XLEN-1) ^ src(1)(XLEN-1) ^ bltuTaken

  private val branchOpTable = List(
    BranchOpType.getBranchType(BranchOpType.beq)  -> beqTaken,
    BranchOpType.getBranchType(BranchOpType.blt)  -> bltTaken,
    BranchOpType.getBranchType(BranchOpType.bltu)  -> bltuTaken
  )
  
  private val taken = LookupTree(BranchOpType.getBranchType(func), branchOpTable) ^ BranchOpType.isBranchInvert(func)
  private val mispredict = (uop.cf.pred_taken ^ taken)

  redirectOutValid := io.in.valid   
  redirectOut := DontCare
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.robIdx := uop.robIdx
  redirectOut.ftqIdx := uop.cf.ftqPtr
  redirectOut.ftqOffset := uop.cf.ftqOffset
  redirectOut.cfiUpdate.isMisPred := mispredict
  redirectOut.cfiUpdate.taken := taken
  redirectOut.cfiUpdate.predTaken := uop.cf.pred_taken
  redirectOut.isException := false.B
  redirectOut.isLoadLoad := false.B
  redirectOut.isLoadStore := false.B
  redirectOut.isFlushPipe := uop.ctrl.flushPipe
  redirectOut.isPreWalk := false.B

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.data := 0.U
}