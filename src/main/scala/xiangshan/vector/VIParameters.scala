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

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import scala.math.min


trait HasVIParameter {

  implicit val p: Parameters


  val XLEN = 64
  val VLEN = 128
  val minFLen = 32
  val fLen = 64
  def xLen = XLEN

  val DecodeWidth = 6
  val RenameWidth = 4
  val CommitWidth = 6
}