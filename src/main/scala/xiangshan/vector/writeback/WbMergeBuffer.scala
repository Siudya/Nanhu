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
    Date: 2023-06-28
    email: guanmingxing@bosc.ac.cn

    Vector Instruction writeback merge
    if mask = 8'b1111_1111  => can write back
    else                    => store it in here
---------------------------------------------------------------------------------------*/

package xiangshan.vector.writeback

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._

import utils._
import xs.utils._
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.rob._
import freechips.rocketchip.diplomacy._

class WbMergeBufferPtr(size: Int) extends CircularQueuePtr[WbMergeBufferPtr](size) with HasCircularQueuePtrHelper
object WbMergeBufferPtr {
  def apply(f: Boolean, v: Int, size: Int): WbMergeBufferPtr = {
    val ptr = Wire(new WbMergeBufferPtr(size))
    ptr.flag := f.B
    ptr.value := v.U
    ptr
  }
}

class WbMergeBufferPtrHelper(size: Int, allocateWidth: Int, releaseWidth: Int)(implicit p: Parameters) 
  extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val allocate        = Vec(allocateWidth, DecoupledIO(new WbMergeBufferPtr(size)))
    val release         = Flipped(ValidIO(UInt(log2Up(size + 1).W)))
    val releasePtrValue = Output(new WbMergeBufferPtr(size))
    val validNum        = Output(UInt(log2Up(size+1).W))
  })

  val allocatePtr = RegInit(WbMergeBufferPtr(false, 0, size))
  val releasePtr  = RegInit(WbMergeBufferPtr(false, 0, size))

  for((port, i) <- io.allocate.zipWithIndex) {
    val allocPtr = allocatePtr + i.U
    port.bits   := allocPtr
    port.valid  := (allocPtr >= releasePtr)
  }
  
  val allocatePtrNext = allocatePtr + PopCount(io.allocate.map(_.fire))
  allocatePtr := allocatePtrNext

  val releasePtrNext = releasePtr + io.release.bits
  releasePtr := Mux(io.release.valid, releasePtrNext, releasePtr)

  io.releasePtrValue := releasePtr
  io.validNum := distanceBetween(allocatePtr, releasePtr)
}

class WbMergeBuffer(size: Int = 64, allocateWidth: Int = 4, mergeWidth: Int = 4, wbWidth: Int = 4)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val redirect    = Flipped(ValidIO(new Redirect))
    val waitqueue   = Vec(allocateWidth, DecoupledIO(new WbMergeBufferPtr(size)))
    val rob         = Vec(wbWidth, ValidIO(new ExuOutput))
    val exu         = Vec(mergeWidth, Flipped(ValidIO(new ExuOutput)))
  })

  val ptrHelper = Module(new WbMergeBufferPtrHelper(size, allocateWidth, wbWidth))

  val mergeTable = Reg(Vec(size, new ExuOutput))

  //allocate, connect with WaitQueue
  //TODO: bypass Merge
  io.waitqueue <> ptrHelper.io.allocate

  for((e, i) <- mergeTable.zipWithIndex) {
    val mergeVec = Wire(Vec(mergeWidth, Bool()))
    mergeVec := io.exu.map(w => w.valid && (w.bits.uop.mergeIdx.value === i.U))
    val mergeWen = mergeVec.asUInt.orR

    val allocateVec = Wire(Vec(allocateWidth, Bool()))
    allocateVec := io.waitqueue.map(w => w.fire && (w.bits.value === i.U))
    val allocateWen = allocateVec.asUInt.orR
    
    when(mergeWen) {
      assert(e.wbmask === 0.U || Mux1H(mergeVec, io.exu.map(_.bits.uop.robIdx)) === e.uop.robIdx)
      e.wbmask                := e.wbmask | Mux1H(mergeVec, io.exu.map(_.bits.wbmask))
      e.uop.robIdx            := Mux1H(mergeVec, io.exu.map(_.bits.uop.robIdx))
      e.vxsat                 := e.vxsat | Mux1H(mergeVec, io.exu.map(_.bits.vxsat))
      e.uop.cf.exceptionVec   := Mux1H(mergeVec, io.exu.map(_.bits.uop.cf.exceptionVec))
    }.elsewhen(allocateWen) {
      e.wbmask := 0.U
      e.vxsat := false.B
    }.otherwise {
      e.wbmask := e.wbmask
    }
  }

  //select writeback, connect with ROB, and release mergeIdx to freeIdxQueue
  val wbVec = Wire(Vec(wbWidth, Bool()))
  
  val wbPtr = Wire(new WbMergeBufferPtr(size))
  wbPtr := ptrHelper.io.releasePtrValue

  wbVec(0) := (ptrHelper.io.validNum.orR) & mergeTable(wbPtr.value).wbmask.andR
  for(i <- 1 until wbWidth) {
    val entry = mergeTable(wbPtr.value + i.U)
    wbVec(i) := wbVec(i-1) & entry.wbmask.andR & (ptrHelper.io.validNum > i.U)
  }

  for((port, i) <- io.rob.zipWithIndex) {
    val entry = mergeTable(wbPtr.value + i.U)
    port.bits   := entry
    port.valid  := wbVec(i)
  }

  ptrHelper.io.release.bits := PopCount(wbVec)
  ptrHelper.io.release.valid := wbVec.asUInt.orR

  //redirect
  //TODO:
  // val cancelVec = Wire(Vec(size, Bool()))
  // val cancelNum = Wire(UInt(log2Up(size + 1).W))
  // for((entry, i) <- mergeTable.zipWithIndex) {
  //     cancelVec(i) := io.redirect.valid && (io.redirect.bits.robIdx <= entry.uop.robIdx) 
  // }
  // cancelNum := PopCount(cancelVec)
  // ptrHelper.io.release.bits := cancelNum
  // ptrHelper.io.release.valid := cancelNum.orR
}
