package xiangshan.backend.execute.fu.mdu

import chisel3._
object MDUOpType {
  // mul
  // bit encoding: | type (2bit) | isWord(1bit) | opcode(2bit) |
  def mul = "b00000".U
  def mulh = "b00001".U
  def mulhsu = "b00010".U
  def mulhu = "b00011".U
  def mulw = "b00100".U
  def mulw7 = "b01100".U
  // div
  // bit encoding: | type (2bit) | isWord(1bit) | isSign(1bit) | opcode(1bit) |
  def div = "b10000".U
  def divu = "b10010".U
  def rem = "b10001".U
  def remu = "b10011".U
  def divw = "b10100".U
  def divuw = "b10110".U
  def remw = "b10101".U
  def remuw = "b10111".U
  def isMul(op: UInt) = !op(4)
  def isDiv(op: UInt) = op(4)
  def isDivSign(op: UInt) = isDiv(op) && !op(1)
  def isW(op: UInt) = op(2)
  def isH(op: UInt) = (isDiv(op) && op(0)) || (isMul(op) && op(1, 0) =/= 0.U)
  def getMulOp(op: UInt) = op(1, 0)
}
