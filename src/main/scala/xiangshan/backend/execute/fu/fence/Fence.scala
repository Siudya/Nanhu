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
import xiangshan.backend.execute.fu.csr.CSRConst.ModeU
import xiangshan.{RedirectLevel, XSBundle}
import xiangshan.backend.execute.fu.{FUWithRedirect, FunctionUnit}

class FenceToSbuffer extends Bundle {
  val flushSb = Output(Bool())
  val sbIsEmpty = Input(Bool())
}

sealed class SfenceBundleBits(implicit p: Parameters) extends XSBundle {
  val rs1 = Bool()
  val rs2 = Bool()
  val addr = UInt(VAddrBits.W)
  val asid = UInt(AsidLength.W)
}
class SfenceBundle(implicit p: Parameters) extends Valid(new SfenceBundleBits) {
  override def toPrintable: Printable = {
    p"valid:0x${Hexadecimal(valid)} rs1:${bits.rs1} rs2:${bits.rs2} addr:${Hexadecimal(bits.addr)}"
  }
}
class FenceIBundle extends Bundle {
  val start = Output(Bool())
  val done = Input(Bool())
}

class Fence(implicit p: Parameters) extends FUWithRedirect {

  val sfence = IO(Output(new SfenceBundle))
  val fencei = IO(new FenceIBundle)
  val toSbuffer = IO(new FenceToSbuffer)
  val disableSfence = IO(Input(Bool()))
  val priviledgeMode = IO(Input(UInt(2.W)))

<<<<<<< HEAD
  val (valid, src1) = (
    io.in.valid,
    io.in.bits.src(0)
  )

  val s_idle :: s_wait :: s_tlb :: s_icache :: s_fence :: s_nofence :: s_wb :: Nil = Enum(7)
=======
  private val s_idle :: s_wait :: s_tlb :: s_icache :: s_fence :: s_nofence :: s_wb :: Nil = Enum(7)
>>>>>>> 12cfcce1bf50f4ac6945b9165c0735a7f466382c

  private val state = RegInit(s_idle)
  /* fsm
   * s_idle    : init state, send sbflush
   * s_wait  : send sbflush, wait for sbEmpty
   * s_tlb   : flush tlb, just hold one cycle
   * s_icache: flush icache, just hold one cycle
   * s_fence : do nothing, for timing optimiaztion
   * s_nofence: do nothing , for Svinval extension
   */

  private val sbuffer = toSbuffer.flushSb
  private val sbEmpty = toSbuffer.sbIsEmpty
  private val uop = RegEnable(io.in.bits.uop, io.in.fire())
  private val valid = RegNext(io.in.fire, false.B)
  private val func = uop.ctrl.fuOpType

  private val instrIllegal = (disableSfence || priviledgeMode === ModeU) && func === FenceOpType.sfence

  // NOTE: icache & tlb & sbuffer must receive flush signal at any time
<<<<<<< HEAD
  sbuffer      := state === s_wait && !(func === FenceOpType.sfence && disableSfence)
=======
  sbuffer      := state === s_wait && !instrIllegal
>>>>>>> 12cfcce1bf50f4ac6945b9165c0735a7f466382c
  fencei.start := state === s_icache
  sfence.valid := state === s_tlb && !disableSfence
  sfence.bits.rs1  := uop.ctrl.imm(4, 0) === 0.U
  sfence.bits.rs2  := uop.ctrl.imm(9, 5) === 0.U
  sfence.bits.addr := RegEnable(io.in.bits.src(0), io.in.fire())
  sfence.bits.asid := RegEnable(io.in.bits.src(1), io.in.fire())

  switch(state){
    is(s_idle){
<<<<<<< HEAD
      when(io.in.valid){ state := s_wait }
=======
      when(valid && !instrIllegal){ state := s_wait }
>>>>>>> 12cfcce1bf50f4ac6945b9165c0735a7f466382c
    }
    is(s_wait){
      when(func === FenceOpType.fencei && sbEmpty){
        state := s_icache
<<<<<<< HEAD
      }.elsewhen(func === FenceOpType.sfence && (sbEmpty || disableSfence)){
=======
      }.elsewhen(func === FenceOpType.sfence && sbEmpty){
>>>>>>> 12cfcce1bf50f4ac6945b9165c0735a7f466382c
        state := s_tlb
      }.elsewhen(func === FenceOpType.fence && sbEmpty){
        state := s_fence
      }.elsewhen(func === FenceOpType.nofence  && sbEmpty){
        state := s_nofence
      }
    }
    is(s_icache){
      when(fencei.done){state := s_wb}
    }
    is(s_tlb){
      state := s_wb
    }
    is(s_fence){
      state := s_wb
    }
    is(s_nofence) {
      state := s_wb
    }
    is(s_wb){
      state := s_idle
    }
  }

  io.in.ready := state === s_idle
<<<<<<< HEAD
  io.out.valid := state === s_wb
=======
  io.out.valid := state === s_wb || (instrIllegal && valid)
>>>>>>> 12cfcce1bf50f4ac6945b9165c0735a7f466382c
  io.out.bits.data := DontCare
  io.out.bits.uop := uop
  io.out.bits.uop.cf.exceptionVec(illegalInstr) := instrIllegal

  redirectOutValid := io.out.valid && uop.ctrl.flushPipe
  redirectOut := DontCare
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.robIdx := uop.robIdx
  redirectOut.ftqIdx := uop.cf.ftqPtr
  redirectOut.ftqOffset := uop.cf.ftqOffset
  redirectOut.cfiUpdate.predTaken := false.B
  redirectOut.cfiUpdate.taken := false.B
  redirectOut.cfiUpdate.isMisPred := false.B
  redirectOut.isException := false.B
  redirectOut.isLoadLoad := false.B
  redirectOut.isLoadStore := false.B
  redirectOut.isFlushPipe := uop.ctrl.flushPipe

  assert(!(io.out.valid && io.out.bits.uop.ctrl.rfWen))
}
