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
package xiangshan.backend.regfile

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import xiangshan._
import xiangshan.backend.decode.{ImmUnion, Imm_LUI_LOAD}
import xiangshan.backend.execute.exucx.ExuComplexParam
import xiangshan.backend.execute.fu.jmp.JumpOpType
import xs.utils.{SignExt, ZeroExt}

object ImmExtractor {
  def apply(cfg: ExuComplexParam, in: ExuInput, pc: Option[UInt] = None, target: Option[UInt] = None, mmuEnable:Option[Bool] = None)
           (implicit p: Parameters): ExuInput = {
    if (cfg.hasJmp) {
      val res = Wire(new ExuInput)
      res := Mux(in.uop.ctrl.fuType === FuType.jmp, JumpImmExtractor(in, pc.get, target.get, mmuEnable.get), AluImmExtractor(in))
      res.uop.cf.pc := pc.get
      res
    } else if (cfg.hasMul) {
      Mux(in.uop.ctrl.fuType === FuType.bku, BkuImmExtractor(in), AluImmExtractor(in))
    } else if (cfg.hasDiv) {
      AluImmExtractor(in)
    } else if (cfg.hasLoad || cfg.hasSpecialLoad) {
      LoadImmExtractor(in)
    } else {
      in
    }
  }
  private def JumpImmExtractor(in:ExuInput, jump_pc:UInt, jalr_target:UInt, mmuEnable:Bool)(implicit p: Parameters):ExuInput = {
    val immExtractedRes = WireInit(in)
    when(SrcType.isPc(in.uop.ctrl.srcType(0))) {
      immExtractedRes.src(0) := Mux(mmuEnable, SignExt(jump_pc, p(XSCoreParamsKey).XLEN), ZeroExt(jump_pc, p(XSCoreParamsKey).XLEN))
    }
    // when src1 is reg (like sfence's asid) do not let data_out(1) be the jalr_target
    when(JumpOpType.jumpOpIsPrefetch_I(in.uop.ctrl.fuOpType)){
      immExtractedRes.src(1) := SignExt(ImmUnion.S.toImm32(in.uop.ctrl.imm), p(XSCoreParamsKey).XLEN)
    }.elsewhen(SrcType.isPcOrImm(in.uop.ctrl.srcType(1))) {
      immExtractedRes.src(1) := jalr_target
    }
    immExtractedRes
  }
  private def AluImmExtractor(in:ExuInput)(implicit p: Parameters):ExuInput = {
    val immExtractedRes = WireInit(in)
    when(SrcType.isImm(in.uop.ctrl.srcType(1))) {
      val imm32 = Mux(in.uop.ctrl.selImm === SelImm.IMM_U,
        ImmUnion.U.toImm32(in.uop.ctrl.imm),
        ImmUnion.I.toImm32(in.uop.ctrl.imm)
      )
      immExtractedRes.src(1) := SignExt(imm32, p(XSCoreParamsKey).XLEN)
    }
    immExtractedRes
  }
  private def BkuImmExtractor(in: ExuInput)(implicit p: Parameters): ExuInput = {
    val immExtractedRes = WireInit(in)
    when(SrcType.isImm(in.uop.ctrl.srcType(1))) {
      val imm32 = ImmUnion.I.toImm32(in.uop.ctrl.imm)
      immExtractedRes.src(1) := SignExt(imm32, p(XSCoreParamsKey).XLEN)
    }
    immExtractedRes
  }
  private def LoadImmExtractor(in: ExuInput)(implicit p: Parameters): ExuInput = {
    val immExtractedRes = WireInit(in)
    when(SrcType.isImm(in.uop.ctrl.srcType(0))) {
      immExtractedRes.src(0) := SignExt(Imm_LUI_LOAD().getLuiImm(in.uop), p(XSCoreParamsKey).XLEN)
    }
    immExtractedRes
  }
}