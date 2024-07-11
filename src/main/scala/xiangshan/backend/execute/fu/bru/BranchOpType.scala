package xiangshan.backend.execute.fu.bru

import chisel3._
import chisel3.util._

object BranchOpType {
  def beq: UInt = "b111_0000".U
  def bne: UInt = "b111_0010".U
  def blt: UInt = "b111_1000".U
  def bge: UInt = "b111_1010".U
  def bltu: UInt = "b111_1100".U
  def bgeu: UInt = "b111_1110".U

  def isBranch(func: UInt): Bool = func(6, 4) === "b111".U
  def getBranchType(func: UInt): UInt = func(3, 2)
  def isBranchInvert(func: UInt): Bool = func(1)
}
