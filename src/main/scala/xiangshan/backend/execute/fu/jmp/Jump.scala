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

package xiangshan.backend.execute.fu.jmp

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.execute.fu.FUWithRedirect
import xiangshan.{FuOpType, RedirectLevel, XSModule}
import xs.utils.{ParallelMux, SignExt}

class JumpDataModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Input(UInt(XLEN.W))
    val pc = Input(UInt(XLEN.W)) // sign-ext to XLEN
    val immMin = Input(UInt(ImmUnion.maxLen.W))
    val func = Input(FuOpType())
    val isRVC = Input(Bool())
    val result, target = Output(UInt(XLEN.W))
    val isAuipc = Output(Bool())
  })
  val (src1, pc, immMin, func, isRVC) = (io.src, io.pc, io.immMin, io.func, io.isRVC)

  val isJalr = JumpOpType.jumpOpIsJalr(func)
  val isAuipc = JumpOpType.jumpOpIsAuipc(func)
  val offset = SignExt(ParallelMux(Seq(
    isJalr -> ImmUnion.I.toImm32(immMin),
    isAuipc -> ImmUnion.U.toImm32(immMin),
    !(isJalr || isAuipc) -> ImmUnion.J.toImm32(immMin)
  )), XLEN)

  val snpc = Mux(isRVC, pc + 2.U, pc + 4.U)
  val target = src1 + offset // NOTE: src1 is (pc/rf(rs1)), src2 is (offset)

  // RISC-V spec for JALR:
  // The target address is obtained by adding the sign-extended 12-bit I-immediate to the register rs1,
  // then setting the least-significant bit of the result to zero.
  io.target := Cat(target(XLEN - 1, 1), false.B)
  io.result := Mux(JumpOpType.jumpOpIsAuipc(func), target, snpc)
  io.isAuipc := isAuipc
}

class Jump(implicit p: Parameters) extends FUWithRedirect {
  val prefetchI = Output(Valid(UInt(XLEN.W)))

  val (src1, jalr_target, pc, immMin, func, uop) = (
    io.in.bits.src(0),
    io.in.bits.src(1)(VAddrBits - 1, 0),
    SignExt(io.in.bits.uop.cf.pc, XLEN),
    io.in.bits.uop.ctrl.imm,
    io.in.bits.uop.ctrl.fuOpType,
    io.in.bits.uop
  )

  private val redirectHit = uop.robIdx.needFlush(io.redirectIn)
  private val valid = io.in.valid
  private val isRVC = uop.cf.pd.isRVC
  private val isPrefetchI = JumpOpType.jumpOpIsPrefetch_I(io.in.bits.uop.ctrl.fuOpType)

  private val jumpDataModule = Module(new JumpDataModule)
  jumpDataModule.io.src := src1
  jumpDataModule.io.pc := pc
  jumpDataModule.io.immMin := immMin
  jumpDataModule.io.func := func
  jumpDataModule.io.isRVC := isRVC

  prefetchI.valid := io.in.valid && isPrefetchI
  prefetchI.bits := io.in.bits.src(0) + io.in.bits.src(1)

  redirectOutValid := valid && !jumpDataModule.io.isAuipc && !isPrefetchI
  redirectOut := DontCare
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.robIdx := uop.robIdx
  redirectOut.ftqIdx := uop.cf.ftqPtr
  redirectOut.ftqOffset := uop.cf.ftqOffset
  redirectOut.cfiUpdate.predTaken := true.B
  redirectOut.cfiUpdate.taken := true.B
  redirectOut.cfiUpdate.target := jumpDataModule.io.target
  redirectOut.cfiUpdate.isMisPred := jumpDataModule.io.target(VAddrBits - 1, 0) =/= jalr_target || !uop.cf.pred_taken
  redirectOut.isException := false.B
  redirectOut.isLoadLoad := false.B
  redirectOut.isLoadStore := false.B
  redirectOut.isFlushPipe := uop.ctrl.flushPipe

  io.in.ready := io.out.ready
  io.out.valid := valid
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.data := jumpDataModule.io.result
}
