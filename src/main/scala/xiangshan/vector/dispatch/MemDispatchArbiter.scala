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

/*--------------------------------------------------------------------------------------
    Author: GMX
    Date: 2023-08-06
    email: guanmingxing@bosc.ac.cn

---------------------------------------------------------------------------------------*/

package xiangshan.vector.dispatch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import utils._
import xiangshan._
import xiangshan.mem.mdp._

import xiangshan.vector._
import xiangshan.backend.rob._

class MemDispatchArbiter(arbWidth: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    val memIn = Vec(arbWidth, Flipped(DecoupledIO(new MicroOp)))
    val vmemIn = Vec(arbWidth, Flipped(DecoupledIO(new MicroOp)))
    val toMem2RS = Vec(arbWidth, DecoupledIO(new MicroOp))
  })
  
  val s_mem :: s_vmem :: Nil = Enum(2)
  val arbState = RegInit(s_mem)

  val memCanDeqVec = Wire(Vec(arbWidth, Bool()))
  val vmemCanDeqVec = Wire(Vec(arbWidth, Bool()))
  val memDeqNum = PopCount(io.memIn.map(_.fire))
  val vmemDeqNum = PopCount(io.vmemIn.map(_.fire))
  val vmemDeqTail = PopCount(io.vmemIn.map(_.fire)) - 1.U

  val vRobIdx = Reg(new RobPtr)

  for(((v, mIn), i) <- memCanDeqVec.zip(io.memIn).zipWithIndex) {
    val isVec = mIn.bits.ctrl.isVector
    if(i == 0) {
      v := mIn.valid && !isVec
    } else {
      val recur_v = VecInit(memCanDeqVec.take(i)).asUInt.andR
      v := mIn.valid && !isVec && recur_v
    }
  }

  for(((v, mIn), i) <- vmemCanDeqVec.zip(io.vmemIn).zipWithIndex) {
    if(i == 0) {
      v := mIn.valid
    } else {
      val recur_v = VecInit(vmemCanDeqVec.take(i)).asUInt.andR
      v := mIn.valid && recur_v && mIn.bits.robIdx === vRobIdx
    }
  }

  val redirectRobIdx = io.redirect.bits.robIdx
  //&& !(io.redirect.valid && redirectRobIdx <= memIn.bits.robIdx)

  io.memIn.zip(io.toMem2RS).zipWithIndex.foreach {
    case ((in, out), i) => {
      val isMem = !in.bits.ctrl.isVector
      val isVMem = in.bits.ctrl.isVector
      val canOut = ((isMem && memCanDeqVec(i)) || (isVMem && PopCount(memCanDeqVec) === i.U)) && in.valid
      in.ready := (arbState===s_mem) && out.ready && canOut
    }
  }

  io.vmemIn.zip(io.toMem2RS).zipWithIndex.foreach {
    case ((in, out), i) => {
      in.ready := (arbState===s_vmem) && out.ready && in.valid && (in.bits.robIdx === vRobIdx)
    }
  }

  val memVecSel = Wire(Vec(arbWidth, Bool()))
  for((in, i) <- io.memIn.zipWithIndex) {
    if(i == 0) {
      memVecSel(i) := in.bits.ctrl.isVector && in.valid
    } else {
      val isVMem = in.bits.ctrl.isVector && in.valid
      val isFirstVMem = isVMem && (PopCount(io.memIn.take(i).map(_.bits.ctrl.isVector)) === 0.U)
      memVecSel(i) := isFirstVMem
    }
  }

  val memHasVec = VecInit(io.memIn.map(req => req.valid && req.bits.ctrl.isVector)).asUInt.orR
  when(arbState === s_mem && memHasVec) {
    val selIn = Mux1H(memVecSel, io.memIn)
    arbState := s_vmem
    vRobIdx := selIn.bits.robIdx
  }
  
  when(arbState === s_vmem) {
    val needFlush = io.redirect.valid && (redirectRobIdx <= vRobIdx)
    val accessTail = (io.vmemIn(vmemDeqTail).bits.uopIdx + 1.U) === io.vmemIn(vmemDeqTail).bits.uopNum
    val stateSwitch = (io.vmemIn(vmemDeqTail).fire && accessTail) || needFlush
    arbState := Mux(stateSwitch, s_mem, s_vmem)
  }

  for(i <- 0 until arbWidth) {
    io.toMem2RS(i).bits := Mux(arbState === s_mem, io.memIn(i).bits, io.vmemIn(i).bits)
    io.toMem2RS(i).valid := Mux(arbState === s_mem, memCanDeqVec(i), vmemCanDeqVec(i))
  }
}
