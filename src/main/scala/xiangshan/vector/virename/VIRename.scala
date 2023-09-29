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

---------------------------------------------------------------------------------------*/

package xiangshan.vector.virename

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import xiangshan._
import utils._

import xiangshan.vector._

class VIRename(implicit p: Parameters) extends VectorBaseModule {
  val io = IO(new Bundle{
    val redirect = Flipped(ValidIO(new Redirect))
    //rename, from waitqueue
    val rename = Vec(VIRenameWidth, new Bundle {
      val in = Flipped(DecoupledIO(new MicroOp))
      val out = DecoupledIO(new MicroOp)
    })
    //commit, from ROB
    val commit = Flipped(new RobCommitIO)
    val debug = Output(Vec(32, UInt(VIPhyRegIdxWidth.W)))
  })

  val freeList        = Module(new VIFreeList)
  val renameTable     = Module(new VIRenameTable)
  val rollBackList    = Module(new VIRollBackList)

  io.debug := renameTable.io.debug

  //-------------------------------------------- Rename --------------------------------------------
  //TODO: !redirect && !walk
  val renameNum = PopCount(io.rename.map(req => req.in.valid && req.in.bits.canRename))
  val canRenameNum = PopCount(freeList.io.allocatePhyReg.map(_.valid))
  val doRename = (canRenameNum >= renameNum) && (!io.redirect.valid) && (!io.commit.isWalk)

  io.rename.map(_.in).zip(freeList.io.allocatePhyReg).zip(io.rename.map(_.out)).foreach {
    case ((rin, alloc), ro) =>
      rin.ready := alloc.valid && ro.ready && doRename
      ro.valid := rin.fire
  }

  //allocate FreeList Ptr, write rat and rollbackList
  for(port <- io.rename) {
    port.out.bits := port.in.bits
  }

  /*  Rename
  *------------------------------------------------
  * FreeList: allocate Pdest
  * RAT: write sRAT, and read old pdest
  * RollBackList: write
  *------------------------------------------------
  */
  io.rename.map(_.out).zip(io.rename.map(_.in)).zipWithIndex.foreach {
    case ((resp, req), i) => {
      val renameEn = req.fire && req.bits.canRename
      val allocPhyIdx = freeList.io.allocatePhyReg(i).bits
      freeList.io.allocatePhyReg(i).ready := renameEn
      // reanme write rat
      renameTable.io.rename(i).in.valid := renameEn
      renameTable.io.rename(i).in.bits.lvd := req.bits.ctrl.ldest
      renameTable.io.rename(i).in.bits.lvs1 := req.bits.ctrl.lsrc(0)
      renameTable.io.rename(i).in.bits.lvs2 := req.bits.ctrl.lsrc(0)
      renameTable.io.rename(i).in.bits.allocIdx := allocPhyIdx

      resp.bits.pdest := Mux(renameEn, allocPhyIdx, Mux(req.bits.ctrl.vdWen, renameTable.io.rename(i).out.pvd, req.bits.pdest))
      resp.bits.psrc(0) := Mux(req.bits.ctrl.srcType(0) === SrcType.vec, renameTable.io.rename(i).out.pvs1, req.bits.psrc(0))
      resp.bits.psrc(1) := Mux(req.bits.ctrl.srcType(1) === SrcType.vec, renameTable.io.rename(i).out.pvs2, req.bits.psrc(1));
      resp.bits.old_pdest := Mux(req.bits.ctrl.vdWen, renameTable.io.rename(i).out.pvd, req.bits.old_pdest);
      resp.bits.vm := renameTable.io.rename(i).out.pmask
    }
  }

  // write rollbacklist
  io.rename.map(_.in).zip(rollBackList.io.rename).zipWithIndex.foreach {
    case ((req, rlb), i) => {
      rlb.valid := req.fire && req.bits.canRename
      rlb.bits.robIdx := req.bits.robIdx.value
      rlb.bits.lrIdx := req.bits.ctrl.ldest
      rlb.bits.oldPrIdx := renameTable.io.rename(i).out.pvd
      rlb.bits.newPrIdx := freeList.io.allocatePhyReg(i).bits
    }
  }

  //-------------------------------------------- TODO: commit & walk --------------------------------------------

  rollBackList.io.commit.rob <> io.commit
  renameTable.io.commit := rollBackList.io.commit.rat

  for((rls, i) <- freeList.io.releasePhyReg.zipWithIndex) {
    rls.valid := rollBackList.io.commit.rat.mask(i)
    rls.bits := Mux(rollBackList.io.commit.rat.doCommit, rollBackList.io.commit.rat.prIdxOld(i), rollBackList.io.commit.rat.prIdxNew(i))
  }
}
