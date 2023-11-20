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
package xiangshan.backend.issue.IntRs
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import xiangshan.{FuType, HasXSParameter, MicroOp, Redirect, SrcState, SrcType, XSCoreParamsKey}
import xiangshan.backend.execute.exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import xiangshan.backend.issue._
import xiangshan.backend.rename.BusyTable
import xiangshan.backend.writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xs.utils.GTimer
import xs.utils.perf.HasPerfLogging

class IntegerReservationStation(implicit p: Parameters) extends LazyModule with HasXSParameter{
  private val entryNum = p(XSCoreParamsKey).intRsDepth
  private val wbNodeParam = WriteBackSinkParam(name = "Integer RS", sinkType = WriteBackSinkType.intRs)
  private val rsParam = RsParam(name = "Integer RS", RsType.int, entryNum)
  require(entryNum % rsParam.bankNum == 0)
  val issueNode = new RsIssueNode(rsParam)
  val wakeupNode = new WriteBackSinkNode(wbNodeParam)
  val dispatchNode = new RsDispatchNode(rsParam)

  lazy val module = new IntegerReservationStationImpl(this, rsParam)
}

class IntegerReservationStationImpl(outer:IntegerReservationStation, param:RsParam) extends LazyModuleImp(outer)
  with HasPerfLogging with HasXSParameter {
  require(param.bankNum == 4)
  require(param.entriesNum % param.bankNum == 0)
  private val issue = outer.issueNode.out.head._1 zip outer.issueNode.out.head._2._2
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2._1)
  private val aluIssue = issue.filter(_._2.hasAlu)
  private val mulIssue = issue.filter(_._2.hasMul)
  private val divIssue = issue.filter(_._2.hasDiv)
  private val jmpIssue = issue.filter(_._2.hasJmp)
  private val miscIssue = issue.filter(_._2.hasMisc)

  private val aluIssuePortNum = issue.count(_._2.hasAlu)
  private val mulIssuePortNum = issue.count(_._2.hasMul)
  private val divIssuePortNum = issue.count(_._2.hasDiv)
  private val jmpIssuePortNum = issue.count(_._2.hasJmp)
  private val miscIssuePortNum = issue.count(_._2.hasMisc)

  require(aluIssue.nonEmpty && aluIssue.length <= param.bankNum && (param.bankNum % aluIssue.length) == 0)
  require(mulIssue.nonEmpty && mulIssue.length <= param.bankNum && (param.bankNum % mulIssue.length) == 0)
  require(divIssue.nonEmpty && divIssue.length <= param.bankNum && (param.bankNum % divIssue.length) == 0)
  require(jmpIssue.nonEmpty && jmpIssue.length <= param.bankNum && (param.bankNum % jmpIssue.length) == 0)
  require(miscIssue.nonEmpty && miscIssue.length <= param.bankNum && (param.bankNum % miscIssue.length) == 0)

  private val issueWidth = issue.length
  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val mulSpecWakeup = Output(Vec(mulIssuePortNum, Valid(new WakeUpInfo)))
    val aluJmpSpecWakeup = Output(Vec(aluIssuePortNum + jmpIssuePortNum, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val integerAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
  })
  require(outer.dispatchNode.in.length == 1)
  private val enq = outer.dispatchNode.in.map(_._1).head

  private val internalAluJmpWakeupSignals = Wire(Vec(aluIssuePortNum + jmpIssuePortNum, Valid(new WakeUpInfo)))
  private val extraAluJmpWakeupSignals = Wire(Vec(aluIssuePortNum + jmpIssuePortNum, Valid(new WakeUpInfo)))
  private val internalMulWakeupSignals = Wire(Vec(mulIssuePortNum, Valid(new WakeUpInfo)))
  io.mulSpecWakeup.zip(internalMulWakeupSignals).foreach({case(a, b) =>
    //Add an pipe here for there is no bypass from mul to load/store units.
    val mulWakeupDelayQueue = Module(new WakeupQueue(1))
    mulWakeupDelayQueue.io.in := b
    a := mulWakeupDelayQueue.io.out
    mulWakeupDelayQueue.io.redirect := io.redirect
    mulWakeupDelayQueue.io.earlyWakeUpCancel.foreach(_ := false.B)
  })

  private val wakeupSignals = VecInit(wakeup.map(_._1).map(elm =>{
    val wkp = Wire(Valid(new WakeUpInfo))
    wkp.valid := elm.valid && elm.bits.wakeupValid
    wkp.bits.pdest := elm.bits.uop.pdest
    wkp.bits.robPtr := elm.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
    wkp.bits.destType := Mux(elm.bits.uop.ctrl.rfWen, SrcType.reg, SrcType.DC)
    wkp
  }))
  private val rsWakeupWidth = (wakeupSignals ++ internalAluJmpWakeupSignals ++ internalMulWakeupSignals ++ extraAluJmpWakeupSignals).length
  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new IntegerReservationBank(entriesNumPerBank, issueWidth, rsWakeupWidth, loadUnitNum))
    mod.io.redirect := io.redirect
    mod.io.wakeup.zip(wakeupSignals ++ internalAluJmpWakeupSignals ++ internalMulWakeupSignals ++ extraAluJmpWakeupSignals).foreach({case(a, b) => a := b})
    mod.io.loadEarlyWakeup := io.loadEarlyWakeup
    mod.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    mod
  })
  private val btWakeupWidth = (wakeupSignals ++ internalAluJmpWakeupSignals ++ internalMulWakeupSignals).length
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("IntAllocNetwork")))
  private val integerBusyTable = Module(new BusyTable(NRPhyRegs, param.bankNum * 2, btWakeupWidth, RenameWidth))
  integerBusyTable.io.allocPregs := io.integerAllocPregs
  integerBusyTable.io.wbPregs.take((wakeupSignals ++ internalMulWakeupSignals).length).zip(wakeupSignals ++ internalMulWakeupSignals).foreach({case(bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.reg
    bt.bits := wb.bits.pdest
  })
  private val busyTableAluWbPorts = integerBusyTable.io.wbPregs.drop((wakeupSignals ++ internalMulWakeupSignals).length)
  private var aluWbPortIdx = 0
  internalAluJmpWakeupSignals.foreach(wb=> {
    val delayValidReg = RegNext(wb.valid, false.B)
    val delayBitsReg = RegEnable(wb.bits, wb.valid)
    val shouldBeCancelled = delayBitsReg.lpv.zip(io.earlyWakeUpCancel).map({case(l,c) => l(0) && c}).reduce(_||_)
    val shouldBeFlushed = delayBitsReg.robPtr.needFlush(io.redirect)
    busyTableAluWbPorts(aluWbPortIdx).valid := delayValidReg && delayBitsReg.destType === SrcType.reg && !shouldBeCancelled && !shouldBeFlushed
    busyTableAluWbPorts(aluWbPortIdx).bits := delayBitsReg.pdest
    extraAluJmpWakeupSignals(aluWbPortIdx).valid := busyTableAluWbPorts(aluWbPortIdx).valid
    extraAluJmpWakeupSignals(aluWbPortIdx).bits := delayBitsReg
    extraAluJmpWakeupSignals(aluWbPortIdx).bits.lpv.foreach(_ := 0.U)
    aluWbPortIdx = aluWbPortIdx + 1
  })

  internalAluJmpWakeupSignals.zip(io.aluJmpSpecWakeup).foreach({case(iwkp, owkp) =>
    owkp.valid := RegNext(iwkp.valid, false.B)
    owkp.bits := RegEnable(iwkp.bits, iwkp.valid)
  })

  private val aluExuCfg = aluIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.alu).head
  private val mulExuCfg = mulIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.mul).head
  private val divExuCfg = divIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.div).head
  private val jmpExuCfg = jmpIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.jmp).head
  private val miscExuCfg = miscIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.misc).head

  private val aluSelectNetwork = Module(new HybridSelectNetwork(param.bankNum, entriesNumPerBank, aluIssuePortNum, aluExuCfg, false, Some(s"IntAluSelNetwork")))
  private val mulSelectNetwork = Module(new HybridSelectNetwork(param.bankNum, entriesNumPerBank, mulIssuePortNum, mulExuCfg, false, Some(s"IntMulSelNetwork")))
  private val jmpSelectNetwork = Module(new HybridSelectNetwork(param.bankNum, entriesNumPerBank, jmpIssuePortNum, jmpExuCfg, false, Some(s"IntJmpSelNetwork")))
  private val divSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, divIssuePortNum, divExuCfg, false, false, false, Some(s"IntDivSelNetwork")))
  private val miscSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, miscIssuePortNum, miscExuCfg, false, false, false, Some(s"IntMiscSelNetwork")))
  divSelectNetwork.io.tokenRelease.get.zip(wakeup.filter(_._2.exuType == ExuType.div).map(_._1)).foreach({ case(sink, source) => sink := source})

  Seq(aluSelectNetwork, mulSelectNetwork, jmpSelectNetwork).foreach(sn => {
    sn.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
      sink := source.io.selectInfo
    })
    sn.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    sn.io.redirect := io.redirect
  })
  Seq(divSelectNetwork, miscSelectNetwork).foreach(sn => {
    sn.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
      sink := source.io.selectInfo
    })
    sn.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    sn.io.redirect := io.redirect
  })

  private var busyTableReadIdx = 0
  allocateNetwork.io.enqFromDispatch.zip(enq).foreach({case(sink, source) =>
    val rport0 = integerBusyTable.io.read(busyTableReadIdx)
    val rport1 = integerBusyTable.io.read(busyTableReadIdx + 1)
    rport0.req := source.bits.psrc(0)
    rport1.req := source.bits.psrc(1)
    sink.valid := source.valid
    sink.bits := source.bits
    sink.bits.srcState(0) := Mux(source.bits.ctrl.srcType(0) === SrcType.reg, rport0.resp, SrcState.rdy)
    sink.bits.srcState(1) := Mux(source.bits.ctrl.srcType(1) === SrcType.reg, rport1.resp, SrcState.rdy)
    source.ready := sink.ready
    busyTableReadIdx = busyTableReadIdx + 2
    when(source.valid){assert(FuType.integerTypes.map(_ === source.bits.ctrl.fuType).reduce(_||_))}
  })

  private val timer = GTimer()
  for(((fromAllocate, toAllocate), rsBank) <- allocateNetwork.io.enqToRs
    .zip(allocateNetwork.io.entriesValidBitVecList)
    .zip(rsBankSeq)){
    toAllocate := rsBank.io.allocateInfo
    rsBank.io.enq.valid := fromAllocate.valid && !io.redirect.valid
    rsBank.io.enq.bits.data := fromAllocate.bits.uop
    rsBank.io.enq.bits.data.debugInfo.enqRsTime := timer + 1.U
    rsBank.io.enq.bits.addrOH := fromAllocate.bits.addrOH
  }

  private var aluJmpWkpPortIdx = 0
  private var mulWkpPortIdx = 0
  private var aluPortIdx = 0
  private var mulPortIdx = 0
  private var divPortIdx = 0
  private var jmpPortIdx = 0
  private var miscPortIdx = 0
  println("\nInteger Reservation Issue Ports Config:")
  for((iss, issuePortIdx) <- issue.zipWithIndex) {
    println(s"Issue Port $issuePortIdx ${iss._2}")
    prefix(iss._2.name + "_" + iss._2.id) {
      val finalSelectInfo = Wire(Decoupled(new SelectResp(param.bankNum, entriesNumPerBank)))
      val issueDriver = Module(new DecoupledPipeline(param.bankNum, entriesNumPerBank))
      issueDriver.io.earlyWakeUpCancel := io.earlyWakeUpCancel
      if(iss._2.hasJmp) {
        finalSelectInfo <> jmpSelectNetwork.io.issueInfo(jmpPortIdx)
        internalAluJmpWakeupSignals(aluJmpWkpPortIdx) := WakeupQueue(jmpSelectNetwork.io.issueInfo(jmpPortIdx), jmpSelectNetwork.cfg.latency, io.redirect, io.earlyWakeUpCancel, p)
        jmpPortIdx = jmpPortIdx + 1
        aluJmpWkpPortIdx = aluJmpWkpPortIdx + 1
      } else {
        val selectRespArbiter = Module(new SelectRespArbiter(param.bankNum, entriesNumPerBank, 2, false))
        finalSelectInfo <> selectRespArbiter.io.out
        selectRespArbiter.io.in(1) <> aluSelectNetwork.io.issueInfo(aluPortIdx)
        internalAluJmpWakeupSignals(aluJmpWkpPortIdx) := WakeupQueue(aluSelectNetwork.io.issueInfo(aluPortIdx), aluSelectNetwork.cfg.latency, io.redirect, io.earlyWakeUpCancel, p)
        aluPortIdx = aluPortIdx + 1
        aluJmpWkpPortIdx = aluJmpWkpPortIdx + 1
        if (iss._2.isAluMul) {
          selectRespArbiter.io.in(0) <> mulSelectNetwork.io.issueInfo(mulPortIdx)
          internalMulWakeupSignals(mulWkpPortIdx) := WakeupQueue(mulSelectNetwork.io.issueInfo(mulPortIdx), mulSelectNetwork.cfg.latency, io.redirect, io.earlyWakeUpCancel, p)
          mulPortIdx = mulPortIdx + 1
          mulWkpPortIdx = mulWkpPortIdx + 1
        } else if (iss._2.isAluDiv) {
          selectRespArbiter.io.in(0) <> divSelectNetwork.io.issueInfo(divPortIdx)
          divPortIdx = divPortIdx + 1
        } else if (iss._2.isAluMisc) {
          selectRespArbiter.io.in(0) <> miscSelectNetwork.io.issueInfo(miscPortIdx)
          miscPortIdx = miscPortIdx + 1
        } else {
          require(false, "Unknown Exu complex!")
        }
        XSPerfAccumulate(s"iss_${issuePortIdx}_${iss._2.name}_conflict", Cat(selectRespArbiter.io.in.map(_.valid)).andR)
      }
      XSPerfAccumulate(s"iss_${issuePortIdx}_${iss._2.name}_issue", finalSelectInfo.fire)

      val rsBankRen = Mux(issueDriver.io.enq.fire, finalSelectInfo.bits.bankIdxOH, 0.U)
      rsBankSeq.zip(rsBankRen.asBools).foreach({ case (rb, ren) =>
        rb.io.issueAddr(issuePortIdx).valid := ren
        rb.io.issueAddr(issuePortIdx).bits := finalSelectInfo.bits.entryIdxOH
      })
      val rsBankRenDelay = RegEnable(rsBankRen, issueDriver.io.enq.valid)

      finalSelectInfo.ready := issueDriver.io.enq.ready
      issueDriver.io.enq.valid := finalSelectInfo.valid
      issueDriver.io.enq.bits.uop := Mux1H(rsBankRenDelay, rsBankSeq.map(_.io.issueUop(issuePortIdx).bits))
      issueDriver.io.enq.bits.selectResp := finalSelectInfo.bits

      iss._1.issue.valid := issueDriver.io.deq.valid
      iss._1.issue.bits.uop := issueDriver.io.deq.bits.uop
      iss._1.issue.bits.src := DontCare
      iss._1.rsIdx.bankIdxOH := issueDriver.io.deq.bits.bankIdxOH
      iss._1.rsIdx.entryIdxOH := issueDriver.io.deq.bits.entryIdxOH
      iss._1.hold := false.B
      iss._1.auxValid := issueDriver.io.deq.valid
      iss._1.specialPsrc := DontCare
      iss._1.specialPsrcType := DontCare
      iss._1.specialPsrcRen := false.B
      issueDriver.io.deq.ready := iss._1.issue.ready
    }
  }
  println("\nInteger Reservation Wake Up Ports Config:")
  wakeup.zipWithIndex.foreach({case((_, cfg), idx) =>
    println(s"Wake Port $idx ${cfg.name} of ${cfg.complexName} #${cfg.id}")
  })
  XSPerfHistogram("issue_num", PopCount(issue.map(_._1.issue.fire)), true.B, 1, issue.length, 1)
  XSPerfHistogram("valid_entries_num", PopCount(Cat(allocateNetwork.io.entriesValidBitVecList)), true.B, 0, param.entriesNum, 4)
}
