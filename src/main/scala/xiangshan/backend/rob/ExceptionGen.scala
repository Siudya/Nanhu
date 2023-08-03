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
package xiangshan.backend.rob

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import xiangshan.{ExceptionVec, Redirect, TriggerCf, XSBundle, XSModule}
import xs.utils.{HasCircularQueuePtrHelper, ParallelOperation, ParallelPriorityMux}

class RobExceptionInfo(implicit p: Parameters) extends XSBundle {
  // val valid = Bool()
  val robIdx = new RobPtr
  val exceptionVec = ExceptionVec()
  val singleStep = Bool() // TODO add frontend hit beneath
  val crossPageIPFFix = Bool()
  val trigger = new TriggerCf

  //  def trigger_before = !trigger.getTimingBackend && trigger.getHitBackend
  //  def trigger_after = trigger.getTimingBackend && trigger.getHitBackend
  def has_exception: Bool = exceptionVec.asUInt.orR || singleStep || trigger.canFire

  def not_commit: Bool = exceptionVec.asUInt.orR || singleStep || trigger.canFire

  // only exceptions are allowed to writeback when enqueue
  def can_writeback: Bool = exceptionVec.asUInt.orR || singleStep || trigger.canFire
}

object ExceptionGen {
  def selectWb2(in0:Valid[RobExceptionInfo], in1:Valid[RobExceptionInfo], p:Parameters):Valid[RobExceptionInfo] = {
    val valid0 = in0.valid
    val valid1 = in1.valid
    val ptr0 = in0.bits.robIdx
    val ptr1 = in1.bits.robIdx
    val validVec = Cat(valid1, valid0)
    val sel = WireInit(true.B)
    switch(validVec) {
      is("b01".U) {
        sel := true.B
      }
      is("b10".U) {
        sel := false.B
      }
      is("b11".U) {
        sel := ptr0 < ptr1
      }
    }
    val res = Mux(sel, in0, in1)
    res
  }
}

class ExceptionGen(wbNum:Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))
    val enq = Vec(RenameWidth, Flipped(ValidIO(new RobExceptionInfo)))
    val wb = Vec(wbNum, Flipped(ValidIO(new RobExceptionInfo)))
    val out = ValidIO(new RobExceptionInfo)
    val state = ValidIO(new RobExceptionInfo)
  })

  private val currentValid = RegInit(false.B)
  private val current = Reg(new RobExceptionInfo)

  private val writebackAfterSuppress = WireInit(io.wb)
  writebackAfterSuppress.zip(io.wb).foreach({case(a, b) =>
    a.valid := b.valid && b.bits.has_exception && !(b.bits.robIdx.needFlush(io.redirect))
  })
  private val writebackGroup = writebackAfterSuppress.map(Pipe(_))
  private val writebackSel = ParallelOperation(writebackGroup, ExceptionGen.selectWb2(_, _, p))

  private val in_enq_valid = VecInit(io.enq.map(e => e.valid && e.bits.has_exception))
  private val realEnqValid = in_enq_valid.asUInt.orR && !io.redirect.valid
  private val enq_valid = RegNext(realEnqValid, false.B)
  private val enq_bits = RegEnable(ParallelPriorityMux(in_enq_valid, io.enq.map(_.bits)), realEnqValid)

  private val s1_out_valid = writebackSel.valid
  private val s1_out_bits = writebackSel.bits

  // s2: compare the input exception with the current one
  // priorities:
  // (1) system reset
  // (2) current is valid: flush, remain, merge, update
  // (3) current is not valid: s1 or enq
  private val current_flush = current.robIdx.needFlush(io.redirect)
  private val s1_flush = s1_out_bits.robIdx.needFlush(io.redirect)
  when (currentValid) {
    when (current_flush) {
      currentValid := Mux(s1_flush, false.B, s1_out_valid)
    }
    when (s1_out_valid && !s1_flush) {
      when (isAfter(current.robIdx, s1_out_bits.robIdx)) {
        current := s1_out_bits
      }.elsewhen (current.robIdx === s1_out_bits.robIdx) {
        current.exceptionVec := (s1_out_bits.exceptionVec.asUInt | current.exceptionVec.asUInt).asTypeOf(ExceptionVec())
        current.singleStep := s1_out_bits.singleStep || current.singleStep
        current.trigger := (s1_out_bits.trigger.asUInt | current.trigger.asUInt).asTypeOf(new TriggerCf)
      }
    }
  }.elsewhen (s1_out_valid && !s1_flush) {
    currentValid := true.B
    current := s1_out_bits
  }.elsewhen (enq_valid && !io.redirect.valid) {
    currentValid := true.B
    current := enq_bits
  }

  io.out.valid   := s1_out_valid || enq_valid && enq_bits.can_writeback
  io.out.bits    := Mux(s1_out_valid, s1_out_bits, enq_bits)
  io.state.valid := currentValid
  io.state.bits  := current

}
