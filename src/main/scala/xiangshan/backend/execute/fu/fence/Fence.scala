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

package xiangshan.backend.execute.fu.fence

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan.{RedirectLevel, XSBundle}
import xiangshan.backend.execute.fu.{FUWithRedirect, FunctionUnit}
import xs.utils.Assertion.xs_assert

class FenceToSbuffer extends Bundle {
  val flushSb = Output(Bool())
  val sbIsEmpty = Input(Bool())
}

class SfenceBundle(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val bits = new Bundle {
    val rs1 = Bool()
    val rs2 = Bool()
    val addr = UInt(VAddrBits.W)
    val asid = UInt(AsidLength.W)
  }

  override def toPrintable: Printable = {
    p"valid:0x${Hexadecimal(valid)} rs1:${bits.rs1} rs2:${bits.rs2} addr:${Hexadecimal(bits.addr)}"
  }
}

class Fence(implicit p: Parameters) extends FUWithRedirect {

  val sfence = IO(Output(new SfenceBundle))
  val fencei = IO(Output(Bool()))
  val toSbuffer = IO(new FenceToSbuffer)
  val disableSfence = IO(Input(Bool()))

  val (valid, src1) = (
    io.in.valid,
    io.in.bits.src(0)
  )

  val s_idle :: s_wait :: s_tlb :: s_icache :: s_fence :: s_nofence :: Nil = Enum(6)

  val state = RegInit(s_idle)
  /* fsm
   * s_idle    : init state, send sbflush
   * s_wait  : send sbflush, wait for sbEmpty
   * s_tlb   : flush tlb, just hold one cycle
   * s_icache: flush icache, just hold one cycle
   * s_fence : do nothing, for timing optimiaztion
   * s_nofence: do nothing , for Svinval extension
   */

  val sbuffer = toSbuffer.flushSb
  val sbEmpty = toSbuffer.sbIsEmpty
  val uop = RegEnable(io.in.bits.uop, io.in.fire())
  val func = uop.ctrl.fuOpType

  // NOTE: icache & tlb & sbuffer must receive flush signal at any time
  sbuffer      := state === s_wait && !(func === FenceOpType.sfence && disableSfence)
  fencei       := state === s_icache
  sfence.valid := state === s_tlb && !disableSfence
  sfence.bits.rs1  := uop.ctrl.imm(4, 0) === 0.U
  sfence.bits.rs2  := uop.ctrl.imm(9, 5) === 0.U
  sfence.bits.addr := RegEnable(io.in.bits.src(0), io.in.fire())
  sfence.bits.asid := RegEnable(io.in.bits.src(1), io.in.fire())

  when (state === s_idle && io.in.valid) { state := s_wait }
  when (state === s_wait && func === FenceOpType.fencei && sbEmpty) { state := s_icache }
  when (state === s_wait && func === FenceOpType.sfence && (sbEmpty || disableSfence)) { state := s_tlb }
  when (state === s_wait && func === FenceOpType.fence  && sbEmpty) { state := s_fence }
  when (state === s_wait && func === FenceOpType.nofence  && sbEmpty) { state := s_nofence }
  when (state =/= s_idle && state =/= s_wait) { state := s_idle }

  io.in.ready := state === s_idle
  io.out.valid := state =/= s_idle && state =/= s_wait
  io.out.bits.data := DontCare
  io.out.bits.uop := uop
  io.out.bits.uop.cf.exceptionVec(illegalInstr) := func === FenceOpType.sfence && disableSfence

  redirectOutValid := io.out.valid && uop.ctrl.flushPipe
  redirectOut := DontCare
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.robIdx := uop.robIdx
  redirectOut.ftqIdx := uop.cf.ftqPtr
  redirectOut.ftqOffset := uop.cf.ftqOffset
  redirectOut.cfiUpdate.predTaken := false.B
  redirectOut.cfiUpdate.taken := false.B
  redirectOut.cfiUpdate.isMisPred := false.B
  redirectOut.isCsr := false.B
  redirectOut.isLoadLoad := false.B
  redirectOut.isLoadStore := false.B
  redirectOut.flushPipe := uop.ctrl.flushPipe

  xs_assert(!(io.out.valid && io.out.bits.uop.ctrl.rfWen))
}
