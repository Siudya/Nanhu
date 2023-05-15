package xiangshan.backend.execute.fu.fence

import chisel3._
object FenceOpType {
  def fence: UInt = "b10000".U
  def sfence: UInt = "b10001".U
  def fencei: UInt = "b10010".U
  def nofence: UInt = "b00000".U
}
