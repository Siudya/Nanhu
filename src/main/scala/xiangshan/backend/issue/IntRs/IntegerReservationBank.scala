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
package xiangshan.backend.issue.IntRs

import chipsalliance.rocketchip.config.Parameters
import xiangshan.backend.issue._
import chisel3._
import chisel3.util._
import xiangshan.{MicroOp, Redirect, SrcState, SrcType}
import xiangshan.backend.issue.IntRs.EntryState.s_ready

class IntegerReservationBank(entryNum:Int, issueWidth:Int, wakeupWidth:Int, loadUnitNum:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))

    val selectInfo = Output(Vec(entryNum, Valid(new SelectInfo)))
    val allocateInfo = Output(UInt(entryNum.W))

    val enq = Input(Valid(new Bundle {
      val addrOH = UInt(entryNum.W)
      val data = new MicroOp
    }))

    val issueAddr = Input(Vec(issueWidth, Valid(UInt(entryNum.W))))
    val issueUop = Output(Vec(issueWidth, Valid(new MicroOp)))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })

  private val statusArray = Module(new IntegerStatusArray(entryNum, issueWidth, wakeupWidth, loadUnitNum))
  private val payloadArray = Module(new PayloadArray(new MicroOp, entryNum, issueWidth, "IntegerPayloadArray"))

  private def EnqToEntry(in: MicroOp): IntegerStatusArrayEntry = {
    val enqEntry = Wire(new IntegerStatusArrayEntry)
    enqEntry.psrc(0) := in.psrc(0)
    enqEntry.psrc(1) := in.psrc(1)
    enqEntry.srcType(0) := in.ctrl.srcType(0)
    enqEntry.srcType(1) := in.ctrl.srcType(1)
    enqEntry.srcState(0) := Mux(in.ctrl.srcType(0) === SrcType.reg, in.srcState(0), SrcState.rdy)
    enqEntry.srcState(1) := Mux(in.ctrl.srcType(1) === SrcType.reg, in.srcState(1), SrcState.rdy)
    enqEntry.pdest := in.pdest
    enqEntry.lpv.foreach(_.foreach(_ := 0.U))
    enqEntry.fuType := in.ctrl.fuType
    enqEntry.rfWen := in.ctrl.rfWen
    enqEntry.fpWen := in.ctrl.fpWen
    enqEntry.robIdx := in.robIdx
    enqEntry.state := s_ready
    enqEntry
  }

  statusArray.io.redirect := io.redirect
  io.selectInfo := statusArray.io.selectInfo
  io.allocateInfo := statusArray.io.allocateInfo
  statusArray.io.enq.valid := io.enq.valid
  statusArray.io.enq.bits.addrOH := io.enq.bits.addrOH
  statusArray.io.enq.bits.data := EnqToEntry(io.enq.bits.data)
  statusArray.io.issue := io.issueAddr
  statusArray.io.wakeup := io.wakeup
  statusArray.io.loadEarlyWakeup := io.loadEarlyWakeup
  statusArray.io.earlyWakeUpCancel := io.earlyWakeUpCancel

  payloadArray.io.write.en := io.enq.valid
  payloadArray.io.write.addr := io.enq.bits.addrOH
  payloadArray.io.write.data := io.enq.bits.data
  payloadArray.io.read.zip(io.issueAddr).zip(io.issueUop).foreach({
    case((port, iAddr), iData) =>{
      port.addr := iAddr.bits
      iData.bits := port.data
      iData.valid := iAddr.valid
      when(iAddr.valid){assert(PopCount(iAddr.bits) === 1.U)}
    }
  })
}
