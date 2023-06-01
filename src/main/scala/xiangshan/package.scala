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

import chisel3._
import chisel3.util._
import xiangshan.backend.execute.exu.ExuConfig
import xiangshan.backend.execute.fu.FuConfig

package object xiangshan {
  object SrcType {
    def reg: UInt = "b000".U
    def pc: UInt = "b001".U
    def imm: UInt = "b001".U
    def fp: UInt = "b010".U
    def vec: UInt = "b011".U
    def default: UInt = "b100".U
    def DC: UInt = imm // Don't Care
    def X: BitPat = BitPat("b??")
    def isReg(srcType: UInt): Bool = srcType === reg
    def isPc(srcType: UInt): Bool = srcType === pc
    def isImm(srcType: UInt): Bool = srcType === imm
    def isFp(srcType: UInt): Bool = srcType === fp
    def isVec(srcType: UInt): Bool = srcType === vec
    def isPcOrImm(srcType: UInt): Bool = srcType === imm
    def isRegOrFp(srcType: UInt): Bool = srcType === reg || srcType === fp
    def regIsFp(srcType: UInt): Bool = srcType === fp
    def apply(): UInt = UInt(3.W)
  }

  object SrcState {
    def busy    = "b0".U
    def rdy     = "b1".U
    // def specRdy = "b10".U // speculative ready, for future use
    def apply() = UInt(1.W)
  }

  object FuType {
    def jmp = "b0000".U
    def i2f = "b0001".U
    def csr = "b0010".U
    def fence = "b0011".U
    def mul = "b0100".U
    def div = "b0101".U
    def alu = "b0110".U
    def bku = "b0111".U
    def mou = "b1000".U // for amo, lr, sc, fence
    def fmac = "b1001".U
    def fmisc = "b1010".U
    def fDivSqrt = "b1011".U
    def ldu = "b1100".U
    def stu = "b1101".U
    def X = BitPat("b????")
    def num = 14
    def apply() = UInt(log2Up(num).W)

    val functionNameMap = Map(
      jmp.litValue -> "jmp",
      i2f.litValue -> "int_to_float",
      csr.litValue -> "csr",
      alu.litValue -> "alu",
      mul.litValue -> "mul",
      div.litValue -> "div",
      fence.litValue -> "fence",
      bku.litValue -> "bku",
      fmac.litValue -> "fmac",
      fmisc.litValue -> "fmisc",
      fDivSqrt.litValue -> "fdiv/fsqrt",
      ldu.litValue -> "load",
      stu.litValue -> "store",
      mou.litValue -> "mou"
    )

    val integerTypes: Seq[UInt] = Seq(jmp, i2f, csr, alu, mul, div, fence, bku, mou)
    val floatingTypes: Seq[UInt] = Seq(fmac, fmisc, fDivSqrt)
    val memoryTypes: Seq[UInt] = Seq(ldu, stu)

    def isIntExu(fuType: UInt): Bool = integerTypes.map(_ === fuType).reduce(_||_)

    def isJumpExu(fuType: UInt): Bool = fuType === jmp

    def isFpExu(fuType: UInt): Bool = floatingTypes.map(_ === fuType).reduce(_||_)

    def isMemExu(fuType: UInt): Bool = memoryTypes.map(_ === fuType).reduce(_||_)

    def isLoadStore(fuType: UInt): Bool = isMemExu(fuType)

    def isStore(fuType: UInt): Bool = fuType === stu

    def isAMO(fuType: UInt): Bool = fuType === mou

    def isFence(fuType: UInt): Bool = fuType === fence
  }

  object FuOpType {
    def apply() = UInt(7.W)
    def X = BitPat("b???????")
  }

  object CommitType {
    def NORMAL = "b000".U  // int/fp
    def BRANCH = "b001".U  // branch
    def LOAD   = "b010".U  // load
    def STORE  = "b011".U  // store

    def apply() = UInt(3.W)
    def isFused(commitType: UInt): Bool = commitType(2)
    def isLoadStore(commitType: UInt): Bool = !isFused(commitType) && commitType(1)
    def lsInstIsStore(commitType: UInt): Bool = commitType(0)
    def isStore(commitType: UInt): Bool = isLoadStore(commitType) && lsInstIsStore(commitType)
    def isBranch(commitType: UInt): Bool = commitType(0) && !commitType(1) && !isFused(commitType)
  }

  object RedirectLevel {
    def flushAfter = "b0".U
    def flush      = "b1".U

    def apply() = UInt(1.W)
    // def isUnconditional(level: UInt) = level(1)
    def flushItself(level: UInt) = level(0)
    // def isException(level: UInt) = level(1) && level(0)
  }

  object ExceptionVec {
    def apply() = Vec(16, Bool())
  }

  object PMAMode {
    def R = "b1".U << 0 //readable
    def W = "b1".U << 1 //writeable
    def X = "b1".U << 2 //executable
    def I = "b1".U << 3 //cacheable: icache
    def D = "b1".U << 4 //cacheable: dcache
    def S = "b1".U << 5 //enable speculative access
    def A = "b1".U << 6 //enable atomic operation, A imply R & W
    def C = "b1".U << 7 //if it is cacheable is configable
    def Reserved = "b0".U

    def apply() = UInt(7.W)

    def read(mode: UInt) = mode(0)
    def write(mode: UInt) = mode(1)
    def execute(mode: UInt) = mode(2)
    def icache(mode: UInt) = mode(3)
    def dcache(mode: UInt) = mode(4)
    def speculate(mode: UInt) = mode(5)
    def atomic(mode: UInt) = mode(6)
    def configable_cache(mode: UInt) = mode(7)

    def strToMode(s: String) = {
      var result = 0.U(8.W)
      if (s.toUpperCase.indexOf("R") >= 0) result = result + R
      if (s.toUpperCase.indexOf("W") >= 0) result = result + W
      if (s.toUpperCase.indexOf("X") >= 0) result = result + X
      if (s.toUpperCase.indexOf("I") >= 0) result = result + I
      if (s.toUpperCase.indexOf("D") >= 0) result = result + D
      if (s.toUpperCase.indexOf("S") >= 0) result = result + S
      if (s.toUpperCase.indexOf("A") >= 0) result = result + A
      if (s.toUpperCase.indexOf("C") >= 0) result = result + C
      result
    }
  }

  object LSUOpType {
    // load pipeline

    // normal load
    // Note: bit(1, 0) are size, DO NOT CHANGE
    // bit encoding: | load 0 | is unsigned(1bit) | size(2bit) |
    def lb       = "b0000".U
    def lh       = "b0001".U
    def lw       = "b0010".U
    def ld       = "b0011".U
    def lbu      = "b0100".U
    def lhu      = "b0101".U
    def lwu      = "b0110".U

    // Zicbop software prefetch
    // bit encoding: | prefetch 1 | 0 | prefetch type (2bit) |
    def prefetch_i = "b1000".U // TODO
    def prefetch_r = "b1001".U
    def prefetch_w = "b1010".U

    def isPrefetch(op: UInt): Bool = op(3)

    // store pipeline
    // normal store
    // bit encoding: | store 00 | size(2bit) |
    def sb       = "b0000".U
    def sh       = "b0001".U
    def sw       = "b0010".U
    def sd       = "b0011".U

    // l1 cache op
    // bit encoding: | cbo_zero 01 | size(2bit) 11 |
    def cbo_zero  = "b0111".U

    // llc op
    // bit encoding: | prefetch 11 | suboptype(2bit) |
    def cbo_clean = "b1100".U
    def cbo_flush = "b1101".U
    def cbo_inval = "b1110".U

    def isCbo(op: UInt): Bool = op(3, 2) === "b11".U

    // atomics
    // bit(1, 0) are size
    // since atomics use a different fu type
    // so we can safely reuse other load/store's encodings
    // bit encoding: | optype(4bit) | size (2bit) |
    def lr_w      = "b000010".U
    def sc_w      = "b000110".U
    def amoswap_w = "b001010".U
    def amoadd_w  = "b001110".U
    def amoxor_w  = "b010010".U
    def amoand_w  = "b010110".U
    def amoor_w   = "b011010".U
    def amomin_w  = "b011110".U
    def amomax_w  = "b100010".U
    def amominu_w = "b100110".U
    def amomaxu_w = "b101010".U

    def lr_d      = "b000011".U
    def sc_d      = "b000111".U
    def amoswap_d = "b001011".U
    def amoadd_d  = "b001111".U
    def amoxor_d  = "b010011".U
    def amoand_d  = "b010111".U
    def amoor_d   = "b011011".U
    def amomin_d  = "b011111".U
    def amomax_d  = "b100011".U
    def amominu_d = "b100111".U
    def amomaxu_d = "b101011".U

    def size(op: UInt) = op(1,0)
  }


  object BTBtype {
    def B = "b00".U  // branch
    def J = "b01".U  // jump
    def I = "b10".U  // indirect
    def R = "b11".U  // return

    def apply() = UInt(2.W)
  }

  object SelImm {
    def IMM_X  = "b0111".U
    def IMM_S  = "b0000".U
    def IMM_SB = "b0001".U
    def IMM_U  = "b0010".U
    def IMM_UJ = "b0011".U
    def IMM_I  = "b0100".U
    def IMM_Z  = "b0101".U
    def INVALID_INSTR = "b0110".U
    def IMM_B6 = "b1000".U

    def X      = BitPat("b????")

    def apply() = UInt(4.W)
  }

  object ExceptionNO {
    def instrAddrMisaligned = 0
    def instrAccessFault    = 1
    def illegalInstr        = 2
    def breakPoint          = 3
    def loadAddrMisaligned  = 4
    def loadAccessFault     = 5
    def storeAddrMisaligned = 6
    def storeAccessFault    = 7
    def ecallU              = 8
    def ecallS              = 9
    def ecallM              = 11
    def instrPageFault      = 12
    def loadPageFault       = 13
    // def singleStep          = 14
    def storePageFault      = 15
    def priorities = Seq(
      breakPoint, // TODO: different BP has different priority
      instrPageFault,
      instrAccessFault,
      illegalInstr,
      instrAddrMisaligned,
      ecallM, ecallS, ecallU,
      storeAddrMisaligned,
      loadAddrMisaligned,
      storePageFault,
      loadPageFault,
      storeAccessFault,
      loadAccessFault
    )
    def all = priorities.distinct.sorted
    def frontendSet = Seq(
      instrAddrMisaligned,
      instrAccessFault,
      illegalInstr,
      instrPageFault
    )
    def partialSelect(vec: Vec[Bool], select: Seq[Int]): Vec[Bool] = {
      val new_vec = Wire(ExceptionVec())
      new_vec.foreach(_ := false.B)
      select.foreach(i => new_vec(i) := vec(i))
      new_vec
    }
    def selectFrontend(vec: Vec[Bool]): Vec[Bool] = partialSelect(vec, frontendSet)
    def selectAll(vec: Vec[Bool]): Vec[Bool] = partialSelect(vec, ExceptionNO.all)
    def selectByFu(vec:Vec[Bool], fuConfig: FuConfig): Vec[Bool] =
      partialSelect(vec, fuConfig.exceptionOut)
    def selectByExu(vec:Vec[Bool], exuConfig: ExuConfig): Vec[Bool] =
      partialSelect(vec, exuConfig.exceptionOut)
    def selectByExu(vec:Vec[Bool], exuConfigs: Seq[ExuConfig]): Vec[Bool] =
      partialSelect(vec, exuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted)
  }
}
