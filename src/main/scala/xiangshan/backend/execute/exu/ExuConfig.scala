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
  def valu = 10
  def vmac = 11
  def vfp = 12
  def vdiv = 13
  def vperm = 14
  def s2v = 15
  def sldu = 16
  def vmask = 17
  def misc = 18

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
    valu -> "valu",
    vfp -> "vfp",
    vdiv -> "vdiv",
    vmac -> "vmac",
    vperm -> "vperm",
    s2v -> "s2v",
    sldu -> "sldu",
    vmask -> "vmask",
    misc -> "misc"
  )

  def intTypes: Seq[Int] = Seq(alu, mul, div, jmp, misc)
  def memTypes: Seq[Int] = Seq(ldu, sta, std, sldu)
  def fpTypes: Seq[Int] = Seq(fmisc, fmac, fdiv)
  def vecTypes: Seq[Int] = Seq(vfp, valu, vperm, vmac, vdiv, s2v)
  def typeToString(in:Int):String = mapping(in)
  def bypassIntList: Seq[Int] = Seq(alu, mul, ldu, jmp)
  def bypassFpList: Seq[Int] = Seq(fmac)
}

case class ExuConfig
(
  name: String,
  id:Int,
  complexName: String,
  fuConfigs: Seq[FuConfig],
  exuType:Int,
  writebackToRob:Boolean,
  writebackToVms:Boolean,
  needToken:Boolean = false,
  speculativeWakeup:Boolean = false,
  throughVectorRf:Boolean = false
){
  private val intFastWkpSeq = Seq(ExuType.alu, ExuType.jmp, ExuType.mul)
  private val fpFastWkpSeq = Seq(ExuType.fmac, ExuType.mul)
  val intSrcNum:Int = fuConfigs.map(_.numIntSrc).max
  val fpSrcNum:Int = fuConfigs.map(_.numFpSrc).max
  val isIntFastWakeup: Boolean = intFastWkpSeq.contains(exuType)
  val isFpFastWakeup: Boolean = fpFastWkpSeq.contains(exuType)
  val latency: Int = fuConfigs.map(_.latency).max
  val exceptionOut: Seq[Int] = fuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted
  val writeIntRf = fuConfigs.map(_.writeIntRf).reduce(_||_)
  val writeFpRf = fuConfigs.map(_.writeFpRf).reduce(_||_)
  val writeVecRf = fuConfigs.map(_.writeVecRf).reduce(_||_)
  val writeFFlags: Boolean = fuConfigs.map(_.writeFflags).reduce(_ || _)

  private val isVector = throughVectorRf
  private val isLs = exuType == ExuType.ldu || exuType == ExuType.sta || exuType == ExuType.std
  val writebackToRegfile = if(isLs) !isVector && (writeIntRf || writeFpRf) else (writeIntRf || writeFpRf)
  val writebackToIntRs = if(isLs) (!isVector && writeIntRf) else (writeIntRf && !isIntFastWakeup)
  val writebackToFpRs = if(isLs) (!isVector && writeFpRf) else (writeFpRf && !isFpFastWakeup)
  val writebackToReorderQueue = writebackToRob
  val writebackToVecRs = writeVecRf || writeIntRf || writeFpRf
  val writebackToMergeStation = writebackToVms
  val writebackToMemRs = ((writeIntRf || writeFpRf) && !isIntFastWakeup && !isFpFastWakeup) || writeVecRf
  val isVldu = isVector && exuType == ExuType.ldu

  val hasRedirectOut = fuConfigs.map(_.hasRedirect).reduce(_||_)
  val isIntType = ExuType.intTypes.contains(exuType)
  val isMemType = ExuType.memTypes.contains(exuType)
  val isFpType = ExuType.fpTypes.contains(exuType)
  val isVecType = ExuType.vecTypes.contains(exuType)
  val willTriggerVrfWkp = fuConfigs.map(_.triggerVrfWakeup).reduce(_||_)
  val bypassIntRegfile = ExuType.bypassIntList.contains(exuType)
  val bypassFpRegfile = ExuType.bypassFpList.contains(exuType)
  val trigger: Boolean = fuConfigs.map(_.trigger).reduce(_ || _)
  val hasException: Boolean = exceptionOut.nonEmpty || trigger

  override def toString = s"\n\t${name}: intSrcNum: ${intSrcNum} fpSrcNum: ${fpSrcNum} Type: ${ExuType.typeToString(exuType)} " +
    "\n\t\t Functions Units: " + fuConfigs.map(_.toString + " ").reduce(_++_)
}
