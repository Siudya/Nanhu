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

/*--------------------------------------------------------------------------------------
    Author: GMX
    Date: 2023-08-11
    email: guanmingxing@bosc.ac.cn

---------------------------------------------------------------------------------------*/

package xiangshan.vector

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

import utils._
import xs.utils._

import xiangshan._
import xiangshan.backend.issue.DqDispatchNode
import xiangshan.backend.rob._
import xiangshan.vector._
import xiangshan.vector.videcode._
import xiangshan.vector.vtyperename._
import xiangshan.vector.viwaitqueue._
import xiangshan.vector.virename._
import xiangshan.vector.dispatch._
import xiangshan.vector.writeback._
import xiangshan.backend.execute.fu.csr.vcsr._

class SIRenameInfo(implicit p: Parameters) extends VectorBaseBundle {
  val psrc = Vec(3, UInt(PhyRegIdxWidth.W))
  val pdest = UInt(PhyRegIdxWidth.W)
  val old_pdest = UInt(PhyRegIdxWidth.W)
}

class VectorCtrlBlock(vecDpWidth: Int, vpDpWidth: Int, memDpWidth: Int)(implicit p: Parameters) extends VectorBaseModule with HasXSParameter {
  val io = IO(new Bundle {
    //val hartId = Input(UInt(8.W))
    //from ctrl decode
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
    val allowIn = Input(Bool())
    //from ctrl rename
    val fromVtpRn = Input(Vec(RenameWidth, new VtpToVCtl))
    //from ctrl rob
    val robPtr = Vec(VIDecodeWidth, Flipped(ValidIO(new RobPtr))) //to wait queue
    val vtypewriteback = Flipped(ValidIO(new VtypeWbIO)) //to wait queue
    val mergeIdAllocate = Vec(VIDecodeWidth, Flipped(DecoupledIO(new WbMergeBufferPtr(VectorMergeBufferDepth)))) //to wait queue
    val commit = Flipped(new RobCommitIO) // to rename
    val redirect = Flipped(ValidIO(new Redirect))
    //from csr vstart
    val vstart = Input(UInt(7.W))

    val vDispatch = Vec(vecDpWidth, DecoupledIO(new MicroOp))
    val vpDispatch = Vec(vpDpWidth, DecoupledIO(new MicroOp))
    val vmemDispath = Vec(memDpWidth, DecoupledIO(new MicroOp))

    val vAllocPregs = Vec(VIRenameWidth, ValidIO(UInt(VIPhyRegIdxWidth.W)))

    val debug = Output(Vec(32, UInt(VIPhyRegIdxWidth.W)))
  })

  val videcode    = Module(new VIDecodeUnit)
  val waitqueue   = Module(new NewWaitQueue)
  val virename    = Module(new VIRename)
  val dispatch    = Module(new VectorDispatchWrapper(vecDpWidth, vpDpWidth, memDpWidth))
  private val redirectDelay_dup_0 = Pipe(io.redirect)
  private val redirectDelay_dup_1 = Pipe(io.redirect)
  private val redirectDelay_dup_2 = Pipe(io.redirect)
  private val redirectDelay_dup_3 = Pipe(io.redirect)

  io.debug := virename.io.debug

  videcode.io.in <> io.in

  waitqueue.io.enq := DontCare

  videcode.io.canOut := waitqueue.io.enq.canAccept
  for (i <- 0 until VIDecodeWidth) {
    waitqueue.io.enq.needAlloc(i) := videcode.io.out(i).valid && videcode.io.out(i).bits.ctrl.isVector && !redirectDelay_dup_0.valid
    waitqueue.io.enq.req(i).valid := videcode.io.out(i).valid && videcode.io.out(i).bits.ctrl.isVector && io.allowIn && !redirectDelay_dup_0.valid
    waitqueue.io.enq.req(i).bits.uop := videcode.io.out(i).bits
    waitqueue.io.enq.req(i).bits.uop.pdest := io.fromVtpRn(i).pdest
    waitqueue.io.enq.req(i).bits.uop.psrc := io.fromVtpRn(i).psrc
    waitqueue.io.enq.req(i).bits.uop.old_pdest := io.fromVtpRn(i).old_pdest
    waitqueue.io.enq.req(i).bits.uop.vCsrInfo := io.fromVtpRn(i).vcsrInfo
    waitqueue.io.enq.req(i).bits.uop.robIdx := io.fromVtpRn(i).robIdx
    waitqueue.io.enq.req(i).bits.vtypeRdy := io.fromVtpRn(i).vtypeRdy
    waitqueue.io.enq.req(i).bits.uop.vtypeRegIdx := io.fromVtpRn(i).vtypeIdx
  }


  waitqueue.io.vstart         := io.vstart
  waitqueue.io.vtypeWbData    := io.vtypewriteback
  waitqueue.io.robin          := io.robPtr
  waitqueue.io.mergeId        <> io.mergeIdAllocate
  waitqueue.io.canRename      := VecInit(virename.io.rename.map(_.in.ready)).asUInt.orR
  waitqueue.io.redirect       := redirectDelay_dup_1

  virename.io.redirect    := redirectDelay_dup_2
  //virename.io.uopIn       <> waitqueue.io.out
  for((vrI, wqO) <- virename.io.rename.map(_.in).zip(waitqueue.io.out)) {
    vrI <> wqO
  }
  virename.io.commit      <> io.commit

  for((rp, dp) <- virename.io.rename.map(_.out) zip dispatch.io.req.uop) {
    rp.ready := dispatch.io.req.canDispatch
    dp.bits := rp.bits
    dp.valid := rp.valid
  }

  for((rp, i) <- virename.io.rename.map(_.out).zipWithIndex) {
    io.vAllocPregs(i).valid := rp.valid && rp.bits.ctrl.vdWen && rp.bits.canRename
    io.vAllocPregs(i).bits := rp.bits.pdest
  }

  dispatch.io.redirect <> redirectDelay_dup_3

  io.vDispatch <> dispatch.io.toVectorCommonRS
  io.vpDispatch <> dispatch.io.toVectorPermuRS
  io.vmemDispath <> dispatch.io.toMem2RS
}
