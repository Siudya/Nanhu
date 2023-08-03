package xiangshan.backend.execute.fu.csr
import chisel3._
object CSROpType {
  def jmp = "b000".U

  def wrt = "b001".U

  def set = "b010".U

  def clr = "b011".U

  def wfi = "b100".U

  def wrti = "b101".U

  def seti = "b110".U

  def clri = "b111".U

  def needAccess(op: UInt): Bool = op(1, 0) =/= 0.U
}
