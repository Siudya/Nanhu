package xiangshan.backend.execute.fu.alu

import chisel3._
import chisel3.util._

object ALUOpType {
  // shift optype
  def slliuw: UInt = "b000_0000".U // slliuw: ZEXT(src1[31:0]) << shamt
  def sll: UInt = "b000_0001".U // sll:     src1 << src2
  def bclr: UInt = "b000_0010".U // bclr:    src1 & ~(1 << src2[5:0])
  def bset: UInt = "b000_0011".U // bset:    src1 | (1 << src2[5:0])
  def binv: UInt = "b000_0100".U // binv:    src1 ^ ~(1 << src2[5:0])
  def srl: UInt = "b000_0101".U // srl:     src1 >> src2
  def bext: UInt = "b000_0110".U // bext:    (src1 >> src2)[0]
  def sra: UInt = "b000_0111".U // sra:     src1 >> src2 (arithmetic)
  def rol: UInt = "b000_1001".U // rol:     (src1 << src2) | (src1 >> (xlen - src2))
  def ror: UInt = "b000_1011".U // ror:     (src1 >> src2) | (src1 << (xlen - src2))
  // RV64 32bit optype
  def addw: UInt = "b001_0000".U // addw:      SEXT((src1 + src2)[31:0])
  def oddaddw: UInt = "b001_0001".U // oddaddw:   SEXT((src1[0] + src2)[31:0])
  def subw: UInt = "b001_0010".U // subw:      SEXT((src1 - src2)[31:0])
  def addwbit: UInt = "b001_0100".U // addwbit:   (src1 + src2)[0]
  def addwbyte: UInt = "b001_0101".U // addwbyte:  (src1 + src2)[7:0]
  def addwzexth: UInt = "b001_0110".U // addwzexth: ZEXT((src1  + src2)[15:0])
  def addwsexth: UInt = "b001_0111".U // addwsexth: SEXT((src1  + src2)[15:0])
  def sllw: UInt = "b001_1000".U // sllw:     SEXT((src1 << src2)[31:0])
  def srlw: UInt = "b001_1001".U // srlw:     SEXT((src1[31:0] >> src2)[31:0])
  def sraw: UInt = "b001_1010".U // sraw:     SEXT((src1[31:0] >> src2)[31:0])
  def rolw: UInt = "b001_1100".U
  def rorw: UInt = "b001_1101".U
  // ADD-op
  def adduw: UInt = "b010_0000".U // adduw:  src1[31:0]  + src2
  def add: UInt = "b010_0001".U // add:     src1        + src2
  def oddadd: UInt = "b010_0010".U // oddadd:  src1[0]     + src2
  def sr29add: UInt = "b010_0100".U // sr29add: src1[63:29] + src2
  def sr30add: UInt = "b010_0101".U // sr30add: src1[63:30] + src2
  def sr31add: UInt = "b010_0110".U // sr31add: src1[63:31] + src2
  def sr32add: UInt = "b010_0111".U // sr32add: src1[63:32] + src2
  def sh1adduw: UInt = "b010_1000".U // sh1adduw: {src1[31:0], 1'b0} + src2
  def sh1add: UInt = "b010_1001".U // sh1add: {src1[62:0], 1'b0} + src2
  def sh2adduw: UInt = "b010_1010".U // sh2add_uw: {src1[31:0], 2'b0} + src2
  def sh2add: UInt = "b010_1011".U // sh2add: {src1[61:0], 2'b0} + src2
  def sh3adduw: UInt = "b010_1100".U // sh3add_uw: {src1[31:0], 3'b0} + src2
  def sh3add: UInt = "b010_1101".U // sh3add: {src1[60:0], 3'b0} + src2
  def sh4add: UInt = "b010_1111".U // sh4add: {src1[59:0], 4'b0} + src2
  // SUB-op: src1 - src2
  def sub: UInt = "b011_0000".U
  def sltu: UInt = "b011_0001".U
  def slt: UInt = "b011_0010".U
  def maxu: UInt = "b011_0100".U
  def minu: UInt = "b011_0101".U
  def max: UInt = "b011_0110".U
  def min: UInt = "b011_0111".U
  // branch
  def beq: UInt = "b111_0000".U
  def bne: UInt = "b111_0010".U
  def blt: UInt = "b111_1000".U
  def bge: UInt = "b111_1010".U
  def bltu: UInt = "b111_1100".U
  def bgeu: UInt = "b111_1110".U
  // misc optype
  def and: UInt = "b100_0000".U
  def andn: UInt = "b100_0001".U
  def or: UInt = "b100_0010".U
  def orn: UInt = "b100_0011".U
  def xor: UInt = "b100_0100".U
  def xnor: UInt = "b100_0101".U
  def orcb: UInt = "b100_0110".U
  def sextb: UInt = "b100_1000".U
  def packh: UInt = "b100_1001".U
  def sexth: UInt = "b100_1010".U
  def packw: UInt = "b100_1011".U
  def revb: UInt = "b101_0000".U
  def rev8: UInt = "b101_0001".U
  def pack: UInt = "b101_0010".U
  def orh48: UInt = "b101_0011".U
  def szewl1: UInt = "b101_1000".U
  def szewl2: UInt = "b101_1001".U
  def szewl3: UInt = "b101_1010".U
  def byte2: UInt = "b101_1011".U
  def andlsb: UInt = "b110_0000".U
  def andzexth: UInt = "b110_0001".U
  def orlsb: UInt = "b110_0010".U
  def orzexth: UInt = "b110_0011".U
  def xorlsb: UInt = "b110_0100".U
  def xorzexth: UInt = "b110_0101".U
  def orcblsb: UInt = "b110_0110".U
  def orcbzexth: UInt = "b110_0111".U

  def isAddw(func: UInt): Bool = func(6, 4) === "b001".U && !func(3) && !func(1)
  def isSimpleLogic(func: UInt): Bool = func(6, 4) === "b100".U && !func(0)
  def logicToLsb(func: UInt): UInt = Cat("b110".U(3.W), func(3, 1), 0.U(1.W))
  def logicToZexth(func: UInt): UInt = Cat("b110".U(3.W), func(3, 1), 1.U(1.W))
  def isBranch(func: UInt): Bool = func(6, 4) === "b111".U
  def getBranchType(func: UInt): UInt = func(3, 2)
  def isBranchInvert(func: UInt): Bool = func(1)
  def apply() = UInt(7.W)
}
