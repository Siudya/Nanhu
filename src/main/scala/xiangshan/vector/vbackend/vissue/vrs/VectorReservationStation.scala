package xiangshan.vector.vbackend.vissue.vrs

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import xiangshan.{FuType, HasXSParameter, MicroOp, Redirect, SrcState, SrcType, XSCoreParamsKey}
import xiangshan.backend.execute.exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import xiangshan.FuType.vdiv
import xiangshan.backend.issue._
import xiangshan.backend.rename.BusyTable
import xiangshan.backend.writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}

class VectorReservationStation(implicit p: Parameters) extends LazyModule with HasXSParameter{
  private val entryNum = vectorParameters.vRsDepth
  private val wbNodeParam = WriteBackSinkParam(name = "Vector RS", sinkType = WriteBackSinkType.vecRs)
  private val rsParam = RsParam(name = "Vector RS", RsType.vec, entryNum)
  require(entryNum % rsParam.bankNum == 0)
  val issueNode = new RsIssueNode(rsParam)
  val wakeupNode = new WriteBackSinkNode(wbNodeParam)
  val dispatchNode = new RsDispatchNode(rsParam)

  lazy val module = new VectorReservationStationImpl(this, rsParam)
}

class VectorReservationStationImpl(outer:VectorReservationStation, param:RsParam) extends LazyModuleImp(outer) with HasXSParameter {
  require(param.bankNum == 4)
  require(param.entriesNum % param.bankNum == 0)
  private val issue = outer.issueNode.out.head._1 zip outer.issueNode.out.head._2._2
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2._1)
  issue.foreach(elm => elm._2.exuConfigs.foreach(elm0 => require(ExuType.vecTypes.contains(elm0.exuType))))

  private val issueWidth = issue.length
  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val intAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val fpAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val vecAllocPregs = Vec(vectorParameters.vRenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
  })
  require(outer.dispatchNode.in.length == 1)
  private val enq = outer.dispatchNode.in.map(_._1).head

  private val wakeupSignals = VecInit(wakeup.map(_._1).map(elm =>{
    val wkp = Wire(Valid(new WakeUpInfo))
    wkp.valid := elm.valid && elm.bits.wakeupValid
    wkp.bits.pdest := elm.bits.uop.pdest
    wkp.bits.robPtr := elm.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
    wkp.bits.destType := MuxCase(SrcType.default, Seq(
      (elm.bits.uop.ctrl.rfWen, SrcType.reg),
      (elm.bits.uop.ctrl.fpWen, SrcType.fp),
      (elm.bits.uop.ctrl.vdWen, SrcType.vec)
    ))
    wkp
  }))
  private val wakeupWidth = wakeupSignals.length
  private val wakeupInt = wakeupSignals.zip(wakeup.map(_._2)).filter(_._2.writeIntRf).map(_._1)
  private val wakeupFp = wakeupSignals.zip(wakeup.map(_._2)).filter(_._2.writeFpRf).map(_._1)
  private val wakeupVec = wakeupSignals.zip(wakeup.map(_._2)).filter(e => e._2.writeVecRf && e._2.throughVectorRf).map(_._1)
  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new VrsBank(entriesNumPerBank, issueWidth, wakeupWidth, loadUnitNum))
    mod.io.redirect := io.redirect
    mod.io.wakeup := wakeupSignals
    mod
  })
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("VectorAllocateNetwork")))
  private val oiq = Module(new OrderedInstructionQueue(param.bankNum, vectorParameters.vRsOIQDepth))
  oiq.io.redirect := io.redirect

  private val integerBusyTable = Module(new BusyTable(NRPhyRegs, param.bankNum, wakeupInt.length, RenameWidth))
  integerBusyTable.io.allocPregs := io.intAllocPregs
  integerBusyTable.io.wbPregs.zip(wakeupInt).foreach({case(bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.reg
    bt.bits := wb.bits.pdest
  })
  private val floatingBusyTable = Module(new BusyTable(NRPhyRegs, param.bankNum, wakeupFp.length, RenameWidth))
  floatingBusyTable.io.allocPregs := io.fpAllocPregs
  floatingBusyTable.io.wbPregs.zip(wakeupFp).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.fp
    bt.bits := wb.bits.pdest
  })
  private val vectorRfSize = coreParams.vectorParameters.vPhyRegsNum
  private val vRenameWidth = coreParams.vectorParameters.vRenameWidth
  private val vectorBusyTable = Module(new BusyTable(vectorRfSize, param.bankNum * 4, wakeupVec.length, vRenameWidth))
  vectorBusyTable.io.allocPregs := io.vecAllocPregs
  vectorBusyTable.io.wbPregs.zip(wakeupVec).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.vec
    bt.bits := wb.bits.pdest
  })


  private val fuTypeList = issue.flatMap(_._2.exuConfigs).filterNot(_.exuType == ExuType.vdiv).flatMap(_.fuConfigs).map(_.fuType)
  private val vdivWb = wakeup.filter(w => w._2.name == "VDivExu").map(_._1)

  private val orderedSelectNetwork = Module(new VrsSelectNetwork(param.bankNum, entriesNumPerBank, issue.length, true, false, 0, fuTypeList, Some(s"VectorOrderedSelectNetwork")))
  private val unorderedSelectNetwork = Module(new VrsSelectNetwork(param.bankNum, entriesNumPerBank, issue.length, false, false, 0, fuTypeList, Some(s"VectorUnorderedSelectNetwork")))
  private val divSelectNetwork = Module(new VrsSelectNetwork(param.bankNum, entriesNumPerBank, issue.length, false, true, vdivWb.length, Seq(vdiv), Some(s"VectorDivSelectNetwork")))
  orderedSelectNetwork.io.orderedCtrl.get := oiq.io.ctrl
  private val selectNetworkSeq = Seq(orderedSelectNetwork, unorderedSelectNetwork, divSelectNetwork)
  selectNetworkSeq.foreach(sn => {
    sn.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
      sink := source.io.selectInfo
    })
    sn.io.redirect := io.redirect
  })

  divSelectNetwork.io.tokenRelease.get.zip(vdivWb).foreach({case(a, b) => a := b})

  private var intBusyTableReadIdx = 0
  private var fpBusyTableReadIdx = 0
  private var vectorBusyTableReadIdx = 0
  allocateNetwork.io.enqFromDispatch.zip(oiq.io.enq).zip(oiq.io.needAlloc).zip(enq).foreach({case(((sink, o_sink), o_alloc), source) =>
    val intReadPort = integerBusyTable.io.read(intBusyTableReadIdx)
    val fpReadPort = floatingBusyTable.io.read(fpBusyTableReadIdx)
    val vecReadPorts = Seq.tabulate(4)(idx => vectorBusyTable.io.read(vectorBusyTableReadIdx + idx))
    intReadPort.req := source.bits.psrc(0)
    fpReadPort.req := source.bits.psrc(0)
    vecReadPorts(0).req := source.bits.psrc(0)
    vecReadPorts(1).req := source.bits.psrc(1)
    vecReadPorts(2).req := source.bits.psrc(2)
    vecReadPorts(3).req := source.bits.vm
    sink.valid := source.valid && oiq.io.enqCanAccept
    sink.bits := source.bits
    sink.bits.srcState(0) := MuxCase(SrcState.rdy, Seq(
      (source.bits.ctrl.srcType(0) === SrcType.reg, intReadPort.resp),
      (source.bits.ctrl.srcType(0) === SrcType.fp, fpReadPort.resp),
      (source.bits.ctrl.srcType(0) === SrcType.vec, vecReadPorts(0).resp)
    ))
    sink.bits.srcState(1) := vecReadPorts(1).resp
    sink.bits.srcState(2) := vecReadPorts(2).resp
    sink.bits.vmState := vecReadPorts(3).resp
    source.ready := sink.ready && oiq.io.enqCanAccept

    o_alloc := source.valid && source.bits.vctrl.ordered
    o_sink.valid := source.valid && source.bits.vctrl.ordered && sink.ready
    o_sink.bits := source.bits
    intBusyTableReadIdx = intBusyTableReadIdx + 1
    fpBusyTableReadIdx = fpBusyTableReadIdx + 1
    vectorBusyTableReadIdx = vectorBusyTableReadIdx + 4
  })

  for(((fromAllocate, toAllocate), rsBank) <- allocateNetwork.io.enqToRs
    .zip(allocateNetwork.io.allocVec)
    .zip(rsBankSeq)){
    toAllocate := rsBank.io.alloc
    rsBank.io.enq.valid := fromAllocate.valid && !io.redirect.valid
    rsBank.io.enq.bits := fromAllocate.bits
  }

  private var orderedPortIdx = 0
  private var unorderedPortIdx = 0
  private var divPortIdx = 0
  println("\nVector Reservation Issue Ports Config:")
  for((iss, issuePortIdx) <- issue.zipWithIndex) {
    println(s"Issue Port $issuePortIdx ${iss._2}")
    prefix(iss._2.name + "_" + iss._2.id) {
      val issueDriver = Module(new VDecoupledPipeline)
      issueDriver.io.redirect := io.redirect

      val finalIssueArbiter = Module(new Arbiter(new VrsSelectResp(param.bankNum, entriesNumPerBank), 3))
      finalIssueArbiter.io.in(0) <> orderedSelectNetwork.io.issueInfo(orderedPortIdx)
      finalIssueArbiter.io.in(1) <> unorderedSelectNetwork.io.issueInfo(unorderedPortIdx)
      finalIssueArbiter.io.in(2) <> divSelectNetwork.io.issueInfo(divPortIdx)

      oiq.io.issued := orderedSelectNetwork.io.issueInfo(orderedPortIdx).fire

      orderedPortIdx = orderedPortIdx + 1
      unorderedPortIdx = unorderedPortIdx + 1
      divPortIdx = divPortIdx + 1

      val finalSelectInfo = finalIssueArbiter.io.out
      val rsBankRen = Mux(issueDriver.io.enq.fire, finalSelectInfo.bits.bankIdxOH, 0.U)
      rsBankSeq.zip(rsBankRen.asBools).foreach({ case (rb, ren) =>
        rb.io.issueAddr(issuePortIdx).valid := ren
        rb.io.issueAddr(issuePortIdx).bits := finalSelectInfo.bits.entryIdxOH
      })

      finalSelectInfo.ready := issueDriver.io.enq.ready
      issueDriver.io.enq.valid := finalSelectInfo.valid
      issueDriver.io.enq.bits := Mux1H(rsBankRen, rsBankSeq.map(_.io.issueUop(issuePortIdx).bits))
      issueDriver.io.enq.bits.robIdx := finalSelectInfo.bits.info.robPtr
      issueDriver.io.enq.bits.uopIdx := finalSelectInfo.bits.info.uopIdx
      issueDriver.io.enq.bits.ctrl.fuType := finalSelectInfo.bits.info.fuType

      iss._1.issue.valid := issueDriver.io.deq.valid
      iss._1.issue.bits.uop := issueDriver.io.deq.bits
      iss._1.issue.bits.src := DontCare
      iss._1.rsIdx.bankIdxOH := DontCare
      iss._1.rsIdx.entryIdxOH := DontCare
      iss._1.specialPsrc := issueDriver.io.specialPsrc
      iss._1.specialPsrcType := issueDriver.io.specialPsrcType
      iss._1.specialPsrcRen := issueDriver.io.specialPsrcRen
      issueDriver.io.deq.ready := iss._1.issue.ready
    }
  }
  println("\nVector Reservation Wake Up Ports Config:")
  wakeup.zipWithIndex.foreach({case((_, cfg), idx) =>
    println(s"Wake Port $idx ${cfg.name} of ${cfg.complexName} #${cfg.id}")
  })
}

