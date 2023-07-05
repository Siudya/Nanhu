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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import xiangshan.backend.exu.ExuConfig
import xs.utils._
import Vector.videcode.VIDecodeUnit
import xiangshan.vector.vtyperename.VtypeRename
import Vector._

abstract class VBundle(implicit val p: Parameters) extends Bundle
  with HasVIParameter

abstract class VIModule(implicit val p: Parameters) extends MultiIOModule
  with HasVIParameter {
  def io: Record
}


class Vector(dpExuConfigs: Seq[Seq[Seq[ExuConfig]]])(implicit p: Parameters) extends LazyModule{

  lazy val module = new VectorImp(this)

}

class VectorImp(outer: Vector)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasCircularQueuePtrHelper
{

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val cpu_halt = Output(Bool())

    //in
    //from ctrl decode
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
    //from ctrl rename
    val vtype = Input(UInt(8.W)) //from rename to vtyperename
    //from ctrl rob
    val allowdeq = Input(Bool()) //to wait queue

    val redirect = Flipped(ValidIO(new Redirect))

    //out
    //to exu
    val dispatch = Vec(2*dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp))

  })

  val videcode = Module(new VIDecodeUnit)
  val vtyperename = Module(new VtypeRename(DecodeWidth,DecodeWidth,))

  for (i <- 0 until DecodeWidth) {
    val renamePipe = PipelineNext(io.in(i), videcode.io.in(i).ready,
      io.redirect.valid)
  }
}
