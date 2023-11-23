/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
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

---------------------------------------------------------------------------------------*/

package xiangshan.vector.virename

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import xiangshan._
import xiangshan.vector._
import xiangshan.backend.rob.RobPtr

import xs.utils._

class VRobEntry(implicit p: Parameters) extends VectorBaseBundle {
  val robIdx = new RobPtr
  val logicRegIdx = UInt(5.W)
  val oldPhyRegIdx = UInt(VIPhyRegIdxWidth.W)
  val newPhyRegIdx = UInt(VIPhyRegIdxWidth.W)
}

class VRob(implicit p: Parameters) extends VectorBaseModule with HasCircularQueuePtrHelper {
  
  val size = VIPhyRegsNum
  
  class VRobPtr extends CircularQueuePtr[VRobPtr](VIPhyRegsNum)
  
  val io = IO(new Bundle {
    val enq = Vec(VIRenameWidth, Flipped(ValidIO(new VRobEntry)))
    val blockRename = Output(Bool())
    val commit = new Bundle {
      val rob = Flipped(new RobCommitIO)
      val rat = Output(new VIRatCommitPort)
    }
    val redirect = Flipped(ValidIO(new Redirect))
  })

  val enqPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  val deqPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  assert(enqPtr >= deqPtr)

  val entryNum = distanceBetween(enqPtr, deqPtr)

  val array = Reg(Vec(size, new VRobEntry))
  val array_v = RegInit(VecInit(Seq.fill(size)(false.B)))

  // enq
  val enqPtrVec = Wire(Vec(VIRenameWidth, UInt(log2Up(size).W)))
  enqPtrVec.zipWithIndex.foreach {
    case (ptr, i) => {
      if(i == 0) {
        ptr := enqPtr.value
      } else {
        val frontValidNum = PopCount(io.enq.take(i).map(_.valid))
        ptr := (enqPtr + frontValidNum).value
      }
    }
  }

  enqPtrVec.zip(io.enq).foreach {
    case (ptr, enq) => {
      when(enq.valid) {
        array(ptr) := enq.bits
        array_v(ptr) := true.B
      }
    }
  }

  // deq
  val flushVec = Wire(Vec(size, Bool()))
  array.zip(array_v).zipWithIndex.foreach {
    case ((e, v), i) => {
      flushVec(i) := v && e.robIdx.needFlush(io.redirect)
    }
  }

  val walkNum = RegInit(0.U(log2Up(size + 1).W))
  when(io.redirect.valid && !io.commit.rat.doWalk) {
    walkNum := PopCount(flushVec)
  }.elsewhen(io.redirect.valid && io.commit.rat.doWalk) {
    walkNum := PopCount(flushVec) - PopCount(io.commit.rat.mask)
  }.elsewhen(io.commit.rat.doWalk) {
    walkNum := walkNum - PopCount(io.commit.rat.mask)
  }

  // commit
  val commitPtrVec = Wire(Vec(8, UInt(log2Up(size).W)))
  val walkPtrVec = Wire(Vec(8, UInt(log2Up(size).W)))
  commitPtrVec.zip(walkPtrVec).zipWithIndex.foreach {
    case ((cptr, wptr), i) => {
      cptr := (deqPtr + i.U).value
      wptr := (enqPtr - (i+1).U).value
    }
  }

  val commitValid = io.commit.rob.isCommit && io.commit.rob.commitValid.asUInt.orR
  val commitRobIdx = Mux1H(io.commit.rob.commitValid, io.commit.rob.robIdx)
  when(commitValid) {
    assert(PopCount(io.commit.rob.commitValid) <= 1.U, "Only one v inst should be walked or committed")
  }

  val deqPtrVec = Mux(commitValid, commitPtrVec, walkPtrVec)
  val deqEntryVec = Wire(Vec(8, new VRobEntry))
  val deqValidVec = Wire(Vec(8, Bool()))
  val deqRobHitVec = Wire(Vec(8, Bool()))
  deqPtrVec.zipWithIndex.foreach {
    case (ptr, i) => {
      deqValidVec(i) := array_v(ptr)
      val entry = array(ptr)
      deqEntryVec(i) := entry
      deqRobHitVec(i) := entry.robIdx === commitRobIdx
    }
  }

  io.commit.rat.doCommit := commitValid
  io.commit.rat.doWalk := walkNum =/= 0.U && !commitValid

  assert(!(io.commit.rat.doCommit && io.commit.rat.doWalk))

  (deqEntryVec).zip(deqRobHitVec).zip(deqValidVec).zipWithIndex.foreach {
    case (((e, hit), v), i) => {
      val actDeq = Mux(commitValid, hit && v, walkNum.orR > i.U)
      io.commit.rat.mask(i) := actDeq
      io.commit.rat.lrIdx(i) := e.logicRegIdx
      io.commit.rat.prIdxNew(i) := e.newPhyRegIdx
      io.commit.rat.prIdxOld(i) := e.oldPhyRegIdx
      when(actDeq) {
        array_v(deqPtrVec(i)) := false.B
      }
    }
  }

  when(commitValid) {
    deqPtr := deqPtr + PopCount(io.commit.rat.mask)
  }

  when(walkNum.orR && io.commit.rat.doWalk) {
    enqPtr := enqPtr - PopCount(io.commit.rat.mask)
  }.otherwise {
    enqPtr := enqPtr + PopCount(io.enq.map(_.valid))
  }

  io.blockRename := walkNum.orR
}