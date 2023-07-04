/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package Vector

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import chisel3.util.BitPat.bitPatToUInt
import Vector._
import _root_.Vector.videcode.{ImmUnion, VectorArithDecode}
import xiangshan.CtrlFlow

class VCtrlSignals(implicit p: Parameters) extends VBundle {
  val srcType = Vec(3, SrcType())
  val lsrc = Vec(3, UInt(5.W))
  val ldest = UInt(5.W)
  val fuType = FuType()
  val fuOpType = FuOpType()
  val vdWen = Bool()
  val rfWen = Bool()
  val fpWen = Bool()
  val isorder = Bool()
  val isWiden = Bool()
  val isNarrow = Bool()
  val Widen = IsWiden
  val Narrow = IsNarrow
  val selImm = SelImm()
  val imm = UInt(ImmUnion.maxLen.W)
  val commitType = CommitType()

  private def allSignals = srcType ++ Seq(fuType, fuOpType, rfWen, fpWen,
    vdWen, isorder, isWiden, isNarrow, selImm)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]): VCtrlSignals = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, VectorArithDecode.decodeDefault, table)
    allSignals zip decoder foreach { case (s, d) => s := d }
    commitType := DontCare
    this
  }

  def decodewn(inst: UInt, table: Iterable[(BitPat, List[BitPat])]): VCtrlSignals = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, VectorArithDecode.decodeDefault, table)
    allSignals zip decoder foreach { case (s, d) => s := d }
    commitType := DontCare
    this
  }

}


class victrlflow(implicit p: Parameters) extends VBundle {
  val cf = new CtrlFlow
  val funct6 = UInt(6.W)
  val funct3 = UInt(3.W)
  val vm = UInt(1.W)
  val vs1_imm = UInt(5.W)
  val widen = Bool()
  val widen2 = Bool()
  val narrow = Bool()
  val narrow_to_1 = Bool()
}

class vicsrinfo(implicit p: Parameters) extends VBundle {
  val ma = UInt(1.W)
  val ta = UInt(1.W)
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)
  val vl = UInt(8.W)
  val vstart = UInt(7.W)
  val vxrm = UInt(2.W)
  val frm = UInt(3.W)
}

class ViCtrl(implicit p: Parameters) extends VBundle {
  val vicf = new victrlflow
  val viinfo = new vicsrinfo
  val visignal = new VCtrlSignals
  val vs1 = UInt(128.W)
  val vs2 = UInt(128.W)
  val rs1 = UInt(64.W)
  val oldvd = UInt(128.W)
  val mask = UInt(128.W)
  val vd = UInt(128.W)
  val vxsat = UInt(1.W)
  val fflags = UInt(5.W)
}
