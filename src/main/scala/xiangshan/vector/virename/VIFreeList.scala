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

    Free List for allocating free physical reg id
---------------------------------------------------------------------------------------*/

package xiangshan.vector.virename

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

import xiangshan._
import utils._
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, CircularShift}

import xiangshan.vector._
import freechips.rocketchip.jtag.JtagState

class VIFreeList(implicit p: Parameters) extends VectorBaseModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    //be different from int and float rename module
    //freeEntry num, determines whether the requests can enter RenameModule
    //connects with RAT, offer PhyRegIndex
    val allocatePhyReg = Vec(VIRenameWidth, DecoupledIO(UInt(VIPhyRegIdxWidth.W)))
    //connects with RollBackList
    val releasePhyReg = Vec(8, Flipped(ValidIO(UInt(VIPhyRegIdxWidth.W))))
  })

  class VIFreeListPtr extends CircularQueuePtr[VIFreeListPtr](VIPhyRegsNum)
  object VIFreeListPtr {
    def apply(f: Boolean, v: Int): VIFreeListPtr = {
      val ptr = Wire(new VIFreeListPtr)
      ptr.flag := f.B
      ptr.value := v.U
      ptr
    }
  }

  //free list
  val freeList_ds = VecInit(Seq.tabulate(VIPhyRegsNum - 32)(i => (i + 32).U(PhyRegIdxWidth.W)) ++ (Seq.tabulate(32)(i => (i).U(PhyRegIdxWidth.W))))
  val freeList = RegInit(freeList_ds)

  //head and tail pointer
  val allocatePtr = RegInit(VIFreeListPtr(false, 0))
  val releasePtr = RegInit(VIFreeListPtr(true, 0))

  //Allocate
  // val freeEntryNum = distanceBetween(tailPtr, headPtr)
  val allocPtrOHVec = Wire(Vec(VIRenameWidth, UInt(VIPhyRegsNum.W)))
  for(i <- 0 until VIRenameWidth) {
    allocPtrOHVec(i) := (allocatePtr + i.U).toOH
  }

  val phyRegCandidates = Wire(Vec(VIRenameWidth, UInt(VIPhyRegIdxWidth.W)))
  phyRegCandidates := allocPtrOHVec.map(sel => Mux1H(sel, freeList))

  // io.canAllocateNum := Mux(freeEntryNum >= VIRenameWidth.U, VIRenameWidth.U, freeEntryNum)
  for((alloc, i) <- io.allocatePhyReg.zipWithIndex) {
    val canAlloc = (allocatePtr + i.U) < releasePtr
    alloc.valid := canAlloc
    alloc.bits := phyRegCandidates(i)
  }

  val allocNum = PopCount(io.allocatePhyReg.map(_.fire))
  val allocPtrNext = allocatePtr + allocNum
  allocatePtr := allocPtrNext

  //Release
  val releaseNum = PopCount(io.releasePhyReg.map(_.valid))
  val releasePtrNext = releasePtr + releaseNum
  releasePtr := releasePtrNext

  for((rls, i) <- io.releasePhyReg.zipWithIndex) {
    val ptr = releasePtr + PopCount(io.releasePhyReg.take(i+1).map(_.valid))
    when(rls.valid === true.B) {
      freeList(ptr.value) := rls.bits
    }
  }
}
