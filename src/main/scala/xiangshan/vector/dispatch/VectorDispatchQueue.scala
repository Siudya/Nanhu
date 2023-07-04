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

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import utils.HasPerfEvents
import xiangshan._
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, ParallelPriorityEncoder, ParallelPriorityMux, UIntToMask}

import xiangshan.vector._

class VectorDispatchQueueBundle(enqNum: Int, deqNum: Int)(implicit p: Parameters) extends VectorBaseBundle {
    val enq = new Bundle {
        // output: dispatch queue can accept new requests
        val canAccept = Output(Bool())
        // input: need to allocate new entries (for address computing)
        val needAlloc = Vec(enqNum, Input(Bool()))
        // input: actually do the allocation (for write enable)
        val req = Vec(enqNum, Flipped(ValidIO(new VectorMicroOP)))
    }
    val deq = Vec(deqNum, DecoupledIO(new MicroOp))
    val redirect = Flipped(ValidIO(new Redirect))
    val dqFull = Output(Bool())
}

class VectorDispatchQueuePayload(entryNum:Int, enqNum:Int, deqNum:Int)(implicit p: Parameters) extends VectorBaseModule {
    val io = IO(new Bundle{
        val w = Input(Vec(enqNum, new Bundle{
            val en: Bool = Bool()
            val addrOH: UInt = UInt(entryNum.W)
            val data: MicroOp = new MicroOp
        }))
        val r = Vec(deqNum, new Bundle{
        val addrOH: UInt = Input(UInt(entryNum.W))
        val data: MicroOp = Output(new MicroOp)
        })
        val redirect: Valid[Redirect] = Input(Valid(new Redirect))
        val flushVec: UInt = Output(UInt(entryNum.W))
    })

    private val array = Reg(Vec(entryNum, new MicroOp))
    for(w <- io.w){
        for((hit, mem) <- w.addrOH.asBools.zip(array)){
            when(w.en && hit){
                mem := w.data
            }
        }
    }
    for(r <- io.r){
        r.data := Mux1H(r.addrOH, array)
    }

    private val redirectHits = array.map(_.robIdx.needFlush(io.redirect))
    io.flushVec := Cat(redirectHits.reverse)
}

