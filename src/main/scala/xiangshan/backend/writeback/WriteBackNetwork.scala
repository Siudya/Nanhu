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
package xiangshan.backend.writeback

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.exu.ExuType
import freechips.rocketchip.diplomacy._
import xiangshan.{ExuOutput, HasXSParameter, MemPredUpdateReq, Redirect, XSCoreParamsKey}
import xiangshan.frontend.Ftq_RF_Components
import difftest._
import xs.utils.GTimer

class WriteBackNetwork(implicit p:Parameters) extends LazyModule {
  val node = new WriteBackNetworkNode
  lazy val module = new WriteBackNetworkImp(this)
}
class WriteBackNetworkImp(outer:WriteBackNetwork)(implicit p:Parameters) extends LazyModuleImp(outer) with HasXSParameter {
  private val wbSources = outer.node.in.map(i => (i._1, i._2._1))
  private val wbSourcesMap = outer.node.in.map(elm => elm._2._1 -> (elm._1, elm._2._1)).toMap
  private val wbSink = outer.node.out

  println("\n\nWriteback Network Info:")
  println(s"Writeback Num: ${wbSources.length}")
  wbSources.foreach(w => print(w._2))
  val io = IO(new Bundle {
    val pcReadAddr = Output(Vec(2, UInt(log2Ceil(FtqSize).W)))
    val pcReadData = Input(Vec(2, new Ftq_RF_Components))
    val redirectOut = Output(Valid(new Redirect))
    val memPredUpdate = Output(Valid(new MemPredUpdateReq))
    val preWalk = Output(Valid(new Redirect))
    val vecFaultOnlyFirst = Flipped(ValidIO(new ExuOutput))
  })
  private val jmpCsrNum = wbSources.count(wb => wb._2.exuType == ExuType.jmp || wb._2.exuType == ExuType.misc)
  private val aluNum = wbSources.count(_._2.exuType == ExuType.alu)
  private val lduNum = wbSources.count(w => (w._2.exuType == ExuType.ldu || w._2.exuType == ExuType.sta) && w._2.writebackToRob)
  private val redirectGen = Module(new RedirectGen(jmpCsrNum, aluNum, lduNum))
  io.pcReadAddr := redirectGen.io.pcReadAddr
  redirectGen.io.pcReadData := io.pcReadData
  io.preWalk := redirectGen.io.preWalk
  private val localRedirectReg = Pipe(redirectGen.io.redirectOut)

  private def PipeWithRedirect(in: Valid[ExuOutput], latency: Int, p: Parameters): Valid[ExuOutput] = {
    require(latency > 0)
    val res = Wire(Valid(new ExuOutput()(p)))
    val realIn = if (latency == 1) in else PipeWithRedirect(in, latency - 1, p)
    val validCond = realIn.valid && !realIn.bits.uop.robIdx.needFlush(localRedirectReg)
    res.valid := RegNext(validCond, false.B)
    res.bits := RegEnable(realIn.bits, validCond)
    res.bits.redirectValid := RegNext(realIn.bits.redirectValid && !realIn.bits.redirect.robIdx.needFlush(localRedirectReg), false.B)
    res.bits.redirect := RegEnable(realIn.bits.redirect, realIn.bits.redirectValid)
    res
  }

  private var jmpRedirectIdx = 0
  private var aluRedirectIdx = 0
  private var memRedirectIdx = 0
  print("\n\nRedirect Info:")
  wbSources.filter(e => e._2.hasRedirectOut && e._2.writebackToRob).foreach(source => {
    if (source._2.exuType == ExuType.jmp || source._2.exuType == ExuType.misc) {
      print(source._2)
      redirectGen.io.jmpWbIn(jmpRedirectIdx) := source._1
      jmpRedirectIdx = jmpRedirectIdx + 1
    } else if (source._2.exuType == ExuType.alu) {
      print(source._2)
      redirectGen.io.aluWbIn(aluRedirectIdx) := source._1
      aluRedirectIdx = aluRedirectIdx + 1
    } else if (source._2.exuType == ExuType.sta || source._2.exuType == ExuType.ldu) {
      print(source._2)
      redirectGen.io.memWbIn(memRedirectIdx) := source._1
      if(source._2.name == "StuExu" && source._2.id == 1) {
        redirectGen.io.memWbIn(memRedirectIdx) := io.vecFaultOnlyFirst
      }
      memRedirectIdx = memRedirectIdx + 1
    } else {
      require(false, "Unexpected redirect out exu!")
    }
  })

  redirectGen.io.redirectIn := localRedirectReg
  io.redirectOut := redirectGen.io.redirectOut
  io.memPredUpdate := redirectGen.io.memPredUpdate
  private val timer = GTimer()
  for ((s, i) <- wbSink.zipWithIndex) {
    val sinkParam = s._2._2
    val source = sinkParam.map(elm => wbSourcesMap(elm))
    val sink = s._1
    print(s"\n\n${s._2._1.name} sinkId #${i}")
    sink.zip(source).foreach({ case (dst, (src, cfg)) =>
      print(cfg)
      val realSrc = WireInit(src)
      if (s._2._1.needWriteback && cfg.speculativeWakeup) {
        val realValid = src.valid
        realSrc.bits.uop := RegEnable(src.bits.uop, realValid)
        realSrc.valid := RegNext(realValid, false.B)
      }
      realSrc.bits.uop.debugInfo.writebackTime := timer
      if (s._2._1.isRob || s._2._1.isVrs || s._2._1.isVprs || s._2._1.isVms || s._2._1.isMemRs && cfg.throughVectorRf) {
        dst := PipeWithRedirect(realSrc, 2, p)
      } else if (s._2._1.isIntRs) {
        if (cfg.isIntType || cfg.isMemType || cfg.isVecType) {
          dst := Pipe(realSrc)
        } else {
          dst := realSrc
        }
      } else if (s._2._1.isFpRs) {
        if (cfg.isFpType || cfg.isMemType || cfg.isVecType) {
          dst := Pipe(realSrc)
        } else {
          dst := realSrc
        }
      } else if (s._2._1.isMemRs) {
        dst := Pipe(realSrc)
      } else if (s._2._1.isVprs || s._2._1.isVrs) {
        if (cfg.isFpType || cfg.isIntType || cfg.isMemType) {
          dst := Pipe(realSrc, 2)
        } else {
          dst := realSrc
        }
      } else {
        dst := realSrc
      }
    })
  }
  println("")

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val fpWb = wbSources.filter(_._2.writeFpRf).map(_._1)
    val intWb = wbSources.filter(_._2.writeIntRf).map(_._1)

    fpWb.foreach(wb => {
      val difftestFpWb = DifftestModule(new DiffFpWriteback(NRPhyRegs))
      difftestFpWb.coreid := p(XSCoreParamsKey).HartId.U
      difftestFpWb.address := wb.bits.uop.pdest
      difftestFpWb.valid := wb.valid && wb.bits.uop.ctrl.fpWen
      difftestFpWb.data := wb.bits.data
    })

    intWb.foreach(wb => {
      val difftestIntWb = DifftestModule(new DiffIntWriteback(NRPhyRegs))
      difftestIntWb.coreid := p(XSCoreParamsKey).HartId.U
      difftestIntWb.address := wb.bits.uop.pdest
      difftestIntWb.valid := wb.valid && wb.bits.uop.ctrl.rfWen
      difftestIntWb.data := wb.bits.data
    })
  }
}
