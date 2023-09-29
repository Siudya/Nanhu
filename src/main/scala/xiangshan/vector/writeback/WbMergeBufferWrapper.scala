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
    Date: 2023-07-10
    email: guanmingxing@bosc.ac.cn

    Vector Instruction writeback merge
    imp with diplomacy
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
import xiangshan.backend.execute.exu.ExuType
import xiangshan.backend.writeback._
import xiangshan.vector._

import freechips.rocketchip.diplomacy._

class WbMergeBufferWrapper(implicit p: Parameters) extends LazyModule with HasXSParameter with HasVectorParameters {
  val wbNodeParam = WriteBackSinkParam(name = "MergeBuffer", sinkType = WriteBackSinkType.vecMs)
  val writebackNode = new WriteBackSinkNode(wbNodeParam)
  lazy val module = new WbMergeBufferWrapperImp(this)
}

class WbMergeBufferWrapperImp(outer:WbMergeBufferWrapper)(implicit p: Parameters) extends LazyModuleImp(outer) with HasVectorParameters {
  val writebackIn = outer.writebackNode.in.head._2._1 zip outer.writebackNode.in.head._1
  val vectorWbNodes = writebackIn
  val vectorWbNodeNum = vectorWbNodes.length

  println(s"wbMergePortsNum: $vectorWbNodeNum")
  for(wb <- vectorWbNodes) {
    val name: String = wb._1.name
    val id = wb._1.id
    println(s"wbMergeNodes: $name, id: $id")
  }
  
  val io = IO(new Bundle {
    val allocate = Vec(VectorMergeAllocateWidth, DecoupledIO(new WbMergeBufferPtr(VectorMergeBufferDepth)))
    val redirect = Flipped(Valid(new Redirect))
    val rob = Vec(VectorMergeWbWidth, ValidIO(new ExuOutput))
  })

  //io.allocate <> outer.module.io.allocate


  val bufferImp = Module(new WbMergeBuffer(VectorMergeBufferDepth, VectorMergeAllocateWidth, vectorWbNodeNum, VectorMergeWbWidth))

  //write back from VectorExu, use Diplomacy
  val vectorWriteBack = vectorWbNodes.map(_._2)
  for((wb, i) <- vectorWriteBack.zipWithIndex) {
    bufferImp.io.exu(i).valid := wb.valid
    bufferImp.io.exu(i).bits := wb.bits
  }

  io.allocate <> bufferImp.io.waitqueue

  bufferImp.io.redirect <> io.redirect
  io.rob <> bufferImp.io.rob
}
