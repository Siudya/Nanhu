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

package xiangshan.vector.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import utils._
import xiangshan._
import xiangshan.mem.mdp._

import xiangshan.vector._

class VectorDispatchFromRenameIO(implicit p: Parameters) extends VectorBaseBundle {
    val req = Input(Vec(VIRenameWidth, new VectorMicroOP))
    val mask = Input(UInt(VIRenameWidth.W))
    //handshake TODO
}

class VectorDispatchToDQ(implicit p: Parameters) extends VectorBaseBundle {
    val doDispatch = Input(Bool())
    val needDispatch = UInt(log2Up(VIRenameWidth + 1).W)
    val req = Vec(VIRenameWidth, new VectorMicroOP)
    val mask = UInt(VIRenameWidth.W)
}

class VectorDispatch(dispatchTypeNum: Int)(implicit p: Parameters) extends VectorBaseModule {
    val io = IO(new Bundle {
        val redirect = Input(Bool())
        val singleStep = Input(Bool())
        //TODO: LFST

        val fromRename = new VectorDispatchFromRenameIO
        val toVectorMemDQ   = new VectorDispatchToDQ
        val toVectorDQ      = new VectorDispatchToDQ
        val toPermutationDQ = new VectorDispatchToDQ
    })

    val type_ds = Vec(VIRenameWidth, Bool())
    val toPermutation = Wire(type_ds)
    val toVectorMem = Wire(type_ds)
    val toVector = Wire(type_ds)

    class VectorInstrSelectNetwork(width: Int) extends RawModule {
        val io = IO(new Bundle {
            val req = Input(Vec(width, new VectorMicroOP))
            val sel = Input(Vec(width, UInt(2.W))) //type
            val toDqMast = Output(Vec(width, Vec(width, Bool())))
        })

        for((selType, i) <- io.sel.zipWithIndex) {
            io.toDqMast(i) := io.req.map(e => e.typeJudge(selType))
        }
    }

    val selNet = new VectorInstrSelectNetwork(VIRenameWidth)
    selNet.io.req := io.fromRename

    //TODO: 
    selNet.io.sel := VecInit(Seq(0.U, 1.U, 2.U, 3.U))
    
    io.toVectorDQ := selNet.io.toDqMast(0)
    io.toVectorMemDQ := selNet.io.toDqMast(1)
    io.toPermutationDQ := selNet.io.toDqMast(2)


}