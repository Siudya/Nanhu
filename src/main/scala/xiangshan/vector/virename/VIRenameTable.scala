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
import xs.utils.CircularShift

import xiangshan.vector._

class VIRatRenameIO(implicit p: Parameters) extends VectorBaseBundle {
  val in = Flipped(ValidIO(new Bundle {
    val lvd = UInt(5.W)
    val lvs1 = UInt(5.W)
    val lvs2 = UInt(5.W)
    val allocIdx = UInt(VIPhyRegIdxWidth.W)
  }))
  val out = Output(new Bundle {
    val pvd = UInt(VIPhyRegIdxWidth.W)
    val pvs1 = UInt(VIPhyRegIdxWidth.W)
    val pvs2 = UInt(VIPhyRegIdxWidth.W)
    val oldPvd = UInt(VIPhyRegIdxWidth.W)
    val pmask = UInt(VIPhyRegIdxWidth.W)
  })
}

class VIRatCommitPort(implicit p: Parameters) extends VectorBaseBundle {
  val doCommit    = Bool()
  val doWalk      = Bool()
  val mask        = UInt(8.W)
  val lrIdx       = Vec(8, UInt(5.W))
  val prIdxOld    = Vec(8, UInt(VIPhyRegIdxWidth.W))
  val prIdxNew    = Vec(8, UInt(VIPhyRegIdxWidth.W))
}

class VIRenameTable(implicit p: Parameters) extends VectorBaseModule {
  val io = IO(new Bundle{
    val rename = Vec(VIRenameWidth, new VIRatRenameIO)
    val commit      = Input(new VIRatCommitPort)
    val debug  = Output(Vec(32, UInt(VIPhyRegIdxWidth.W))) //for difftest
  })
  //RAT
  val rat_ds = VecInit.tabulate(32)(i => i.U(VIPhyRegIdxWidth.W))
  val sRAT = RegInit(rat_ds)
  val aRAT = RegInit(rat_ds)

  io.debug := aRAT

  //read
  // for((port, i) <- io.renameReadPorts.zipWithIndex) {
  //   port.vs1.prIdx := (0 until i).foldLeft(sRAT(port.vs1.lrIdx))((p, k) => 
  //     Mux((io.renameWritePort.mask(k) === true.B && io.renameWritePort.lrIdx(k) === io.renameReadPorts(i).vs1.lrIdx && io.renameWritePort.doRename === true.B), 
  //       io.renameWritePort.prIdx(k), p))
  //   port.vs2.prIdx := (0 until i).foldLeft(sRAT(port.vs2.lrIdx))((p, k) => 
  //     Mux((io.renameWritePort.mask(k) === true.B && io.renameWritePort.lrIdx(k) === io.renameReadPorts(i).vs2.lrIdx && io.renameWritePort.doRename === true.B), 
  //       io.renameWritePort.prIdx(k), p))
  //   port.vd.prIdx := (0 until i).foldLeft(sRAT(port.vd.lrIdx))((p, k) => 
  //     Mux((io.renameWritePort.mask(k) === true.B && io.renameWritePort.lrIdx(k) === io.renameReadPorts(i).vd.lrIdx && io.renameWritePort.doRename === true.B), 
  //       io.renameWritePort.prIdx(k), p))
  //   port.vmask := (0 until i).foldLeft(sRAT(0.U))((p, k) => 
  //     Mux((io.renameWritePort.mask(k) === true.B && io.renameWritePort.lrIdx(k) === 0.U && io.renameWritePort.doRename === true.B), 
  //       io.renameWritePort.prIdx(k), p))
  // }

  for(((pi, po), bypassNum) <- io.rename.map(_.in).zip(io.rename.map(_.out)).zipWithIndex) {
    po.pvd := pi.bits.allocIdx
    po.pvs1 := sRAT(pi.bits.lvs1)
    po.pvs2 := sRAT(pi.bits.lvs2)
    po.pmask := sRAT(0)
    po.oldPvd := sRAT(pi.bits.lvd)
    if(bypassNum != 0) {
      val renamePorts = io.rename.take(bypassNum)
      val wmasks = renamePorts.map(_.in.valid)
      val wlrs = renamePorts.map(_.in.bits.lvd)
      val wprs = renamePorts.map(_.in.bits.allocIdx)
      Seq(pi.bits.lvs1, pi.bits.lvs2, 0.U, pi.bits.lvd).zip(
        Seq(po.pvs1, po.pvs2, po.pmask, po.oldPvd)
      ).foreach {
        case(lr, pr) => {
          for(i <- 0 until bypassNum) {
            val hit = wmasks(i) && (wlrs(i) === lr)
            when(hit) {
              pr := wprs(i)
            }
          }
        }
      }
    }
  }

  //old regId read, for rollBackList storage

  val renameVec = Wire(Vec(32, Bool()))
  val renamePr = Wire(Vec(32, UInt(VIPhyRegIdxWidth.W)))
  renameVec.zip(renamePr).zipWithIndex.foreach {
    case ((wen, pr), i) => {
      val hitVec = VecInit(io.rename.map(req => req.in.valid && req.in.bits.lvd===i.U))
      wen := hitVec.asUInt.orR
      pr := Mux1H(hitVec, io.rename.map(_.in.bits.allocIdx))
    }
  }

  val walkVec = Wire(Vec(32, Bool()))
  val walkPr = Wire(Vec(32, UInt(VIPhyRegIdxWidth.W)))
  walkVec.zip(walkPr).zipWithIndex.foreach {
    case ((wen, pr), i) => {
      val hitVec = Wire(Vec(8, Bool()))
      io.commit.lrIdx.zip(hitVec).zipWithIndex.foreach {
        case ((lr, hit), j) => {
          hit := lr === i.U && io.commit.doWalk && io.commit.mask(j)
        }
      }
      wen := hitVec.asUInt.orR
      pr := Mux1H(hitVec, io.commit.prIdxOld)
    }
  }

  val sratWen = Mux(io.commit.doWalk, walkVec, renameVec)
  val sratWdata = Mux(io.commit.doCommit, walkPr, renamePr)

  sratWen.zip(sratWdata).zip(sRAT).foreach {
    case ((wen, data), e) => {
      when(wen) {
        e := data
      }
    }
  }

  val commitVec = Wire(Vec(32, Bool()))
  val commitPr = Wire(Vec(32, UInt(VIPhyRegIdxWidth.W)))
  commitVec.zip(commitPr).zipWithIndex.foreach {
    case ((wen, pr), i) => {
      val hitVec = Wire(Vec(8, Bool()))
      io.commit.lrIdx.zip(hitVec).zipWithIndex.foreach {
        case ((lr, hit), j) => {
          hit := lr === i.U && io.commit.doCommit && io.commit.mask(j)
        }
      }
      wen := hitVec.asUInt.orR
      pr := Mux1H(hitVec, io.commit.prIdxOld)
    }
  }

  commitVec.zip(commitPr).zip(aRAT).foreach {
    case ((wen, data), e) => {
      when(wen) {
        e := data
      }
    }
  }

}
