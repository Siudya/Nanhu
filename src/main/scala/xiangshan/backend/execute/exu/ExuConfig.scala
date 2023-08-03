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
/***************************************************************************************
 * Author: Liang Sen
 * E-mail: liangsen20z@ict.ac.cn
 * Date: 2023-06-19
 ****************************************************************************************/
package xiangshan.backend.execute.exu

import xiangshan.backend.execute.fu.FuConfig
import chisel3._
object ExuType{
  def jmp = 0
  def alu = 1
  def mul = 2
  def div = 3
  def ldu = 4
  def sta = 5
  def std = 6
  def fmisc = 7
  def fmac = 8
  def fdiv = 9
  def vred = 10
  def vmisc = 11
  def vfp = 12
  def vint = 13

  private val mapping = Map(
    jmp -> "jmp",
    alu -> "alu",
    mul -> "mul",
    div -> "div",
    ldu -> "ldu",
    sta -> "sta",
    std -> "std",
    fmisc -> "fmisc",
    fmac -> "fmac",
    fdiv -> "fdiv",
    vred -> "vred",
    vmisc -> "vmisc",
    vfp -> "vfp",
    vint -> "vint"
  )

  def intTypes: Seq[Int] = Seq(jmp, alu, mul, div)
  def memTypes: Seq[Int] = Seq(ldu, sta, std)
  def fpTypes: Seq[Int] = Seq(fmisc, fmac, fdiv)
  def vecTypes: Seq[Int] = Seq(vred, vmisc, vfp, vint)
  def typeToString(in:Int):String = mapping(in)
  def bypassIntList: Seq[Int] = Seq(alu, mul, ldu)
  def bypassFpList: Seq[Int] = Seq(ldu)
}

case class ExuConfig
(
  name: String,
  id:Int,
  complexName: String,
  fuConfigs: Seq[FuConfig],
  exuType:Int,
  needToken:Boolean = false,
  speculativeWakeup:Boolean = false
){
  val intSrcNum:Int = fuConfigs.map(_.numIntSrc).max
  val fpSrcNum:Int = fuConfigs.map(_.numFpSrc).max
  val hasFastWakeup: Boolean = fuConfigs.map(_.latency).max != Int.MaxValue
  val latency: Int = fuConfigs.map(_.latency).max
  val exceptionOut: Seq[Int] = fuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted
  val writeIntRf = fuConfigs.map(_.writeIntRf).reduce(_||_)
  val writeFpRf = fuConfigs.map(_.writeFpRf).reduce(_||_)
  val writeVecRf = fuConfigs.map(_.writeVecRf).reduce(_||_)
  val writeFFlags = fuConfigs.map(_.writeFflags).reduce(_||_)
  val wakeUpIntRs = fuConfigs.map(_.writeIntRf).reduce(_||_) && !hasFastWakeup
  val wakeUpFpRs = fuConfigs.map(_.writeFpRf).reduce(_||_)
  val wakeUpMemRs =  fuConfigs.map(e => e.writeIntRf || e.writeFpRf).reduce(_||_) && !hasFastWakeup
  val hasRedirectOut = fuConfigs.map(_.hasRedirect).reduce(_||_)
  val isIntType = ExuType.intTypes.contains(exuType)
  val isMemType = ExuType.memTypes.contains(exuType)
  val isFpType = ExuType.fpTypes.contains(exuType)
  val isVecType = ExuType.vecTypes.contains(exuType)
  val bypassIntRegfile = ExuType.bypassIntList.contains(exuType)
  val bypassFpRegfile = ExuType.bypassFpList.contains(exuType)
  val trigger: Boolean = fuConfigs.map(_.trigger).reduce(_ || _)
  val hasException: Boolean = exceptionOut.nonEmpty || trigger

  override def toString = s"\n\t${name}: intSrcNum: ${intSrcNum} fpSrcNum: ${fpSrcNum} Type: ${ExuType.typeToString(exuType)} " +
    "\n\t\t Functions Units: " + fuConfigs.map(_.toString + " ").reduce(_++_)
}
