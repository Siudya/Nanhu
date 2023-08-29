/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
  * Date: 2023-03-31
  ****************************************************************************************/
package xiangshan.backend.issue
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.exu.ExuConfig
import xiangshan.backend.rob.RobPtr
import xiangshan.{FuType, Redirect, XSBundle, XSModule}
import xs.utils.{LogicShiftRight, ParallelOperation}

class SelectInfo(implicit p: Parameters) extends XSBundle{
  val fuType = FuType()
  val lpv = Vec(loadUnitNum, UInt(LpvLength.W))
  val pdest = UInt(PhyRegIdxWidth.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val robPtr = new RobPtr
}

class SelectResp(val bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends XSBundle {
  val info = new SelectInfo
  val entryIdxOH = UInt(entryIdxWidth.W)
  val bankIdxOH = UInt(bankIdxWidth.W)
}

class SelectPolicy(width:Int, oldest:Boolean, haveEqual:Boolean)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(width, Valid(new RobPtr)))
    val out = Output(Valid(UInt(width.W)))
  })
  private val ostr = if(oldest)"o" else "p"
  private val esrt = if(haveEqual) "e" else ""
  override val desiredName:String = s"SelectPolicy_w${width}" + ostr + esrt
  if(oldest) {
    val onlyOne = PopCount(io.in.map(_.valid)) === 1.U
    val oldestOHMatrix = io.in.zipWithIndex.map({ case (self, idx) =>
      io.in.zipWithIndex.filterNot(_._2 == idx).map(i => (i._1.valid && self.valid && (self.bits <= i._1.bits)) ^ i._1.valid)
    })
    val oldestOHSeq = oldestOHMatrix.map(_.reduce(_|_)).map(!_)
    val oldestOH = if(haveEqual) PriorityEncoderOH(Cat(oldestOHSeq.reverse)) else Cat(oldestOHSeq.reverse)
    val defaultValue = Cat(io.in.map(_.valid).reverse)
    io.out.valid := io.in.map(_.valid).reduce(_ | _)
    io.out.bits := Mux(onlyOne, defaultValue, oldestOH)
  } else {
    io.out.valid := io.in.map(_.valid).reduce(_ | _)
    io.out.bits := PriorityEncoderOH(Cat(io.in.map(_.valid).reverse))
  }
  when(io.out.valid) {
    assert(PopCount(io.out.bits) === 1.U)
  }
}
object SelectPolicy {
  def apply(in:Seq[Valid[SelectResp]], oldest:Boolean, haveEqual:Boolean, bankNum:Int, entryNum:Int, redirect: Valid[Redirect], earlyWakeUpCancel:Vec[Bool], p:Parameters) :Valid[SelectResp] = {
    val selector = Module(new SelectPolicy(in.length, oldest, haveEqual)(p))
    val redirectVec = Cat(in.map(_.bits.info.robPtr.needFlush(redirect)).map(!_).reverse)
    val cancelVec = Cat(in.map(_.bits.info.lpv.zip(earlyWakeUpCancel).map({case(l, c)=>l(0) & c}).reduce(_|_)).map(!_).reverse)
    selector.io.in.zip(in).foreach({case(a, b) =>
      a.valid := b.valid
      a.bits := b.bits.info.robPtr
    })
    val res = Wire(Valid(new SelectResp(bankNum, entryNum)(p)))
    res.valid := selector.io.out.valid && (selector.io.out.bits & redirectVec & cancelVec).orR
    res.bits := Mux1H(selector.io.out.bits, in.map(_.bits))
    res
  }
}
/** {{{
  * Module Name: SelectedNetwork
  *
  * Function Description:
  *   Select ready and supported instructions from several
  *   reservation station banks.
  *
  * Parameters:
  *   bankNum:
  *     The number of banks.
  *   entryNum:
  *     The number of entries in a bank.
  *   issueNum:
  *     The number of issue port for a certain type of instructions.
  *   fuTypeList:
  *     The list of function unit types, to which the selected
  *     instructions should be sent.
  *
  * IO:
  *   selectInfo: [Input][Vec]
  *     The necessary information for selection, which comes from
  *     reservation station banks.
  *   issueInfo: [Output][Vec][Valid]
  *     info:
  *       The information of selected instruction.
  *     bankIdxOH:
  *       The one hot format bank index of selected instruction.
  *     entryIdxOH:
  *       The one hot format entry index of selected instruction.
  *   tokenRelease: [Optional][UInt]
  *     Bit vector to release token.
  *     Some FU may need uncertain clocks to finished its job.
  *     Issuing an instruction of these kind of FUs consumes 1 token.
  *     Stop issuing instructions when running out of tokens.
  *     After these FUs finished a job, release a token to token allocator.
  *     We replace valid-ready handshake with token-release handshake to
  *     cut off ready signal propagation.
  * }}}
*/

class SelectNetwork(bankNum:Int, entryNum:Int, issueNum:Int, val cfg:ExuConfig, oldest:Boolean, haveEqual:Boolean, name:Option[String] = None)(implicit p: Parameters) extends XSModule {
  require(issueNum <= bankNum && 0 < issueNum && bankNum % issueNum == 0, "Illegal number of issue ports are supported now!")
  private val fuTypeList = cfg.fuConfigs.map(_.fuType)
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val selectInfo = Input(Vec(bankNum,Vec(entryNum, Valid(new SelectInfo))))
    val issueInfo = Vec(issueNum, Decoupled(new SelectResp(bankNum, entryNum)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val tokenRelease = if(cfg.needToken)Some(Input(Vec(issueNum,Valid(UInt(PhyRegIdxWidth.W))))) else None
  })
  override val desiredName:String = name.getOrElse("SelectNetwork")

  private val selectInputPerBank = io.selectInfo.zipWithIndex.map({case(si, bidx) =>
    si.zipWithIndex.map({ case (in, eidx) =>
      val selInfo = Wire(Valid(new SelectResp(bankNum, entryNum)))
      selInfo.valid := in.valid && cfg.fuConfigs.map(_.fuType === in.bits.fuType).reduce(_ | _)
      selInfo.bits.info := in.bits
      selInfo.bits.bankIdxOH := (1 << bidx).U(bankNum.W)
      selInfo.bits.entryIdxOH := (1 << eidx).U(entryNum.W)
      selInfo
    })
  })

  private val finalSelectResult = Wire(Vec(issueNum, Valid(new SelectResp(bankNum, entryNum))))
  private val bankNumPerIss = bankNum / issueNum
  finalSelectResult.zipWithIndex.foreach({case(res, i) =>
    val selBanks = selectInputPerBank.slice(i * bankNumPerIss, i * bankNumPerIss + bankNumPerIss).reduce(_ ++ _)
    res := SelectPolicy(selBanks, oldest, haveEqual, bankNum, entryNum, io.redirect, io.earlyWakeUpCancel, p)
  })

  if(cfg.needToken){
    val tokenAllocators = Seq.fill(issueNum)(Module(new TokenAllocator(PhyRegIdxWidth, cfg.fuConfigs.length)))
    for ((((outPort, driver), ta), tr) <- io.issueInfo.zip(finalSelectResult).zip(tokenAllocators).zip(io.tokenRelease.get)) {
      ta.io.redirect := io.redirect
      ta.io.alloc.valid := outPort.fire
      ta.io.alloc.bits.pdest := driver.bits.info.pdest
      ta.io.alloc.bits.robPtr := driver.bits.info.robPtr
      ta.io.alloc.bits.lpv := outPort.bits.info.lpv
      ta.io.earlyWakeUpCancel := io.earlyWakeUpCancel
      ta.io.release := tr
      outPort.valid := driver.valid && ta.io.allow
      outPort.bits.bankIdxOH := driver.bits.bankIdxOH
      outPort.bits.entryIdxOH := driver.bits.entryIdxOH
      outPort.bits.info := driver.bits.info
      outPort.bits.info.lpv.zip(driver.bits.info.lpv).foreach({case(o, i) => o := LogicShiftRight(i, 1)})
    }
  } else {
    for ((outPort, driver) <- io.issueInfo.zip(finalSelectResult)) {
      outPort.valid := driver.valid
      outPort.bits.bankIdxOH := driver.bits.bankIdxOH
      outPort.bits.entryIdxOH := driver.bits.entryIdxOH
      outPort.bits.info := driver.bits.info
      outPort.bits.info.lpv.zip(driver.bits.info.lpv).foreach({case(o, i) => o := LogicShiftRight(i, 1)})
    }
  }
}