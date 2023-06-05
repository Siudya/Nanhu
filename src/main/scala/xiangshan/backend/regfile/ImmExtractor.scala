package xiangshan.backend.regfile

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import xiangshan._
import xiangshan.backend.decode.{ImmUnion, Imm_LUI_LOAD}
import xiangshan.backend.execute.exucx.ExuComplexParam
import xs.utils.SignExt

object ImmExtractor {
  def apply(cfg: ExuComplexParam, in: ExuInput, pc: Option[UInt] = None, target: Option[UInt] = None)
           (implicit p: Parameters): ExuInput = {
    if (cfg.hasJmp) {
      Mux(in.uop.ctrl.fuType === FuType.jmp, JumpImmExtractor(in, pc.get, target.get), AluImmExtractor(in))
    } else if (cfg.hasMul) {
      Mux(in.uop.ctrl.fuType === FuType.bku, BkuImmExtractor(in), AluImmExtractor(in))
    } else if (cfg.hasDiv) {
      AluImmExtractor(in)
    } else if (cfg.hasLoad) {
      LoadImmExtractor(in)
    } else {
      in
    }
  }
  private def JumpImmExtractor(in:ExuInput, jump_pc:UInt, jalr_target:UInt)(implicit p: Parameters):ExuInput = {
    val immExtractedRes = WireInit(in)
    immExtractedRes.uop.cf.pc := jump_pc
    when(SrcType.isPc(in.uop.ctrl.srcType(0))) {
      immExtractedRes.src(0) := SignExt(jump_pc, p(XSCoreParamsKey).XLEN)
    }
    // when src1 is reg (like sfence's asid) do not let data_out(1) be the jalr_target
    when(SrcType.isPcOrImm(in.uop.ctrl.srcType(1))) {
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