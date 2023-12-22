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

package xiangshan.vector

import chisel3.util._
import org.chipsalliance.cde.config._
import xiangshan.HasXSParameter

case class VectorParameters (
    //decode
    vDecodeWidth: Int       = 4,
    //vector instr rename
    vRenameWidth: Int       = 4,
    vCommitWidth: Int       = 4,
    vPhyRegsNum: Int        = 48,
    vWaitQueueNum: Int      = 16,
    vVtypeRegsNum: Int      = 8,
    //dispatch
    vDispatchQueueMem: Int      = 12,
    vDispatchQueuePermu: Int    = 12,
    vDispatchQueueCommon: Int   = 12,
    vDispatchTypeNum: Int = 3,
    //rs
    vRsDepth: Int       = 16,
    vRsOIQDepth: Int    = 8,
    vPRsDepth: Int      = 4,
    //merge
    vMergeBufferDepth: Int = 24,
    vMergeWbWdith: Int = 2
) {
    val vLen: Int = 128 //maybe 64、256、512...
    val vMergeBufferAllocateWidth: Int = vDecodeWidth
    def vPhyRegIdxWidth: Int = log2Up(vPhyRegsNum)
}

case object VectorParametersKey extends Field[VectorParameters](VectorParameters())

trait HasVectorParameters extends HasXSParameter {
    //implicit val p: Parameters
    
    //val vector = p(VectorParametersKey)

    val vectorParams = vectorParameters

    val VLEN = vectorParams.vLen
    val VIDecodeWidth       = vectorParams.vDecodeWidth
    val VIRenameWidth       = vectorParams.vRenameWidth
    val VICommitWidth       = vectorParams.vCommitWidth
    val VIPhyRegsNum        = vectorParams.vPhyRegsNum
    val VIPhyRegIdxWidth    = vectorParams.vPhyRegIdxWidth
    val VIWaitQueueWidth    = vectorParams.vWaitQueueNum
    val VIVtypeRegsNum      = vectorParams.vVtypeRegsNum

    val VectorDispatchCommonWidth = vectorParams.vDispatchQueueCommon
    val VectorDispatchMemWidth = vectorParams.vDispatchQueueMem
    val VectorDispatchPermuWidth = vectorParams.vDispatchQueuePermu
    val VectorDispatchTypeNum = vectorParams.vDispatchTypeNum

    val VectorLMULMax = 8
    val VectorSEWMin = 8
    val MemVectorInstructionMax = VLEN * VectorLMULMax / VectorSEWMin

    val VectorMergeBufferDepth = vectorParams.vMergeBufferDepth
    val VectorMergeAllocateWidth = vectorParams.vMergeBufferAllocateWidth
    val VectorMergeWbWidth = vectorParams.vMergeWbWdith
}
