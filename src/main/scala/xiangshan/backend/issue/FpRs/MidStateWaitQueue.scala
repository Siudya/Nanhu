package xiangshan.backend.issue.FpRs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.Redirect
import xiangshan.backend.issue.SelectResp

class MidStateWaitQueue(latency:Int, bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))
    val in = Input(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
    val out = Output(Valid(new SelectResp( bankIdxWidth, entryIdxWidth)))
    val earlyWakeUp = Output(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
  })
  private val validRegs = Array.fill(latency)(RegInit(false.B))
  private val dataRegs = Array.fill(latency)(Reg(new SelectResp(bankIdxWidth, entryIdxWidth)))
  private val validVec = io.in.valid +: validRegs
  private val dataVec = io.in.bits +: dataRegs
  for(idx <- validVec.indices){
    if(idx == 1){
      validVec(idx) := validVec(idx - 1)
      when(validVec(idx - 1)) {
        dataVec(idx) := dataVec(idx - 1)
      }
    } else if(idx > 1){
      val valid = validVec(idx - 1) & !dataVec(idx - 1).info.robPtr.needFlush(io.redirect)
      validVec(idx) := valid
      when(valid) {
        dataVec(idx) := dataVec(idx - 1)
      }
    }
  }
  io.out.valid := validRegs.last
  io.out.bits := dataRegs.last
  io.earlyWakeUp.valid := validRegs.takeRight(2).head
  io.earlyWakeUp.bits := dataRegs.takeRight(2).head
}
