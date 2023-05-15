package xiangshan.backend.execute.fu.jmp

import chisel3._
object JumpOpType {
  def jal: UInt = "b00".U
  def jalr: UInt = "b01".U
  def auipc: UInt = "b10".U
  //    def call = "b11_011".U
  //    def ret  = "b11_100".U
  def jumpOpisJalr(op: UInt): Bool = op(0)
  def jumpOpisAuipc(op: UInt): Bool = op(1)
}
