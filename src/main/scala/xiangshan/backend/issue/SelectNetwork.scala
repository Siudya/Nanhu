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
class SelectMux(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in0 = Input(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
    val in1 = Input(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
    val out = Output(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
  })
  private val valid0 = io.in0.valid
  private val valid1 = io.in1.valid
  private val ptr0 = io.in0.bits.info.robPtr
  private val ptr1 = io.in1.bits.info.robPtr
  private val validVec = Cat(valid1, valid0)
  private val sel = Mux(validVec === "b01".U, true.B, Mux(validVec === "b10".U, false.B, Mux(validVec === "b11".U, ptr0 < ptr1, true.B)))
  private val res = Mux(sel, io.in0, io.in1)
  io.out := res
}
object SelectMux{
  def apply(in0: Valid[SelectResp], in1: Valid[SelectResp], bankIdxWidth:Int, entryIdxWidth:Int, p: Parameters):Valid[SelectResp] = {
    val smux = Module(new SelectMux(bankIdxWidth, entryIdxWidth)(p))
    smux.io.in0 := in0
    smux.io.in1 := in1
    smux.io.out
  }
}

class Selector(bankNum:Int, entryNum:Int, inputWidth:Int)(implicit p: Parameters) extends Module{
  private val bankIdxWidth = bankNum
  private val entryIdxWidth = entryNum
  val io = IO(new Bundle{
    val in = Input(Vec(inputWidth, Valid(new SelectResp(bankIdxWidth, entryIdxWidth))))
    val out = Output(Valid(new SelectResp(bankIdxWidth, entryIdxWidth)))
  })
  private val operationFunction = SelectMux(_, _, bankIdxWidth, entryIdxWidth, p)
  private val res  = ParallelOperation(io.in, operationFunction)
  io.out := res
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

class SelectNetwork(bankNum:Int, entryNum:Int, issueNum:Int, val cfg:ExuConfig, name:Option[String] = None)(implicit p: Parameters) extends XSModule {
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

  private val issueValidBitVecList = io.selectInfo.map(_.map(info => info.valid & (Cat(fuTypeList.map(_ === info.bits.fuType)).orR)))
  private val issueDataVecList = io.selectInfo.map(_.map(_.bits))
  private val issueBankIdxVecList = io.selectInfo.indices.map(idx => Seq.fill(entryNum)((1<<idx).U(bankNum.W)))
  private val issueEntryIdxVecList = io.selectInfo.indices.map(idx => Seq.tabulate(entryNum)(idx0 => (1<<idx0).U(entryNum.W)))
  private val issueAllDataList = issueValidBitVecList.zip(issueDataVecList).zip(issueBankIdxVecList).zip(issueEntryIdxVecList).map({
    case(((v, d),bi),ei) => v.zip(d).zip(bi).zip(ei)
  })

  private val bankNumPerSelector = bankNum / issueNum
  private val selectorSeq = Seq.fill(issueNum)(Module(new Selector(bankNum, entryNum, bankNumPerSelector * entryNum)))

  private val selectorInput = Seq.tabulate(issueNum)({idx =>
    issueAllDataList.slice(idx*bankNumPerSelector, idx*bankNumPerSelector + bankNumPerSelector).reduce(_++_)
  })

  for((s, si) <- selectorSeq zip selectorInput){
    s.io.in.zip(si).foreach({case(inPort, driver) =>
      inPort.valid := driver._1._1._1
      inPort.bits.info := driver._1._1._2
      inPort.bits.bankIdxOH := driver._1._2
      inPort.bits.entryIdxOH := driver._2
    })
  }

  if(cfg.needToken){
    val tokenAllocators = Seq.fill(issueNum)(Module(new TokenAllocator(PhyRegIdxWidth, cfg.fuConfigs.length)))
    for ((((outPort, driver), ta), tr) <- io.issueInfo.zip(selectorSeq).zip(tokenAllocators).zip(io.tokenRelease.get)) {
      ta.io.redirect := io.redirect
      ta.io.alloc.valid := outPort.fire
      ta.io.alloc.bits.pdest := driver.io.out.bits.info.pdest
      ta.io.alloc.bits.robPtr := driver.io.out.bits.info.robPtr
      ta.io.release := tr
      val shouldBeFlushed = driver.io.out.bits.info.robPtr.needFlush(io.redirect)
      val shouldBeCancelled = driver.io.out.bits.info.lpv.zip(io.earlyWakeUpCancel).map({case(l, c)=>l(0) & c}).reduce(_|_)
      outPort.valid := driver.io.out.valid && ta.io.allow && !shouldBeCancelled && !shouldBeFlushed
      outPort.bits.bankIdxOH := driver.io.out.bits.bankIdxOH
      outPort.bits.entryIdxOH := driver.io.out.bits.entryIdxOH
      outPort.bits.info := driver.io.out.bits.info
      outPort.bits.info.lpv.zip(driver.io.out.bits.info.lpv).foreach({case(o, i) => o := LogicShiftRight(i, 1)})
    }
  } else {
    for ((outPort, driver) <- io.issueInfo.zip(selectorSeq)) {
      val shouldBeFlushed = driver.io.out.bits.info.robPtr.needFlush(io.redirect)
      val shouldBeCancelled = driver.io.out.bits.info.lpv.zip(io.earlyWakeUpCancel).map({case(l, c)=>l(0) & c}).reduce(_|_)
      outPort.valid := driver.io.out.valid && !shouldBeCancelled && !shouldBeFlushed
      outPort.bits.bankIdxOH := driver.io.out.bits.bankIdxOH
      outPort.bits.entryIdxOH := driver.io.out.bits.entryIdxOH
      outPort.bits.info := driver.io.out.bits.info
      outPort.bits.info.lpv.zip(driver.io.out.bits.info.lpv).foreach({case(o, i) => o := LogicShiftRight(i, 1)})
    }
  }
}