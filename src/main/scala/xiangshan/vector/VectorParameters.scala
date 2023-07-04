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
import chipsalliance.rocketchip.config._

case class VectorParameters (
    vLen: Int               = 128, //maybe 64、256、512...
    vDecodeWidth: Int       = 6,
    vRenameWidth: Int       = 4,
    vCommitWidth: Int       = 4,
    vPhyRegsNum: Int        = 64,
    viWalkRobIdxQueueWidth: Int = 64
) {
    def vPhyRegIdxWidth: Int = log2Up(vPhyRegsNum + 1)
}

case object VectorParametersKey extends Field[VectorParameters](VectorParameters())

trait HasVectorParameters {
    implicit val p: Parameters
    
    val vector = p(VectorParametersKey)

    val VLEN = vector.vLen
    val VIDecodeWidth = vector.vDecodeWidth
    val VIRenameWidth = vector.vRenameWidth
    val VICommitWidth = vector.vCommitWidth
    val VIPhyRegsNum = vector.vPhyRegsNum
    val VIPhyRegIdxWidth = vector.vPhyRegIdxWidth

    val VIWalkRobIdxQueueWidth = vector.viWalkRobIdxQueueWidth

    //unit debug only
    val RobSize = 64
}
