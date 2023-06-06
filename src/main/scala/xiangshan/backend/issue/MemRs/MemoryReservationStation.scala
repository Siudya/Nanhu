package xiangshan.backend.issue.MemRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import xiangshan.{FuType, HasXSParameter, MicroOp, Redirect, SrcState, SrcType, XSCoreParamsKey}
import xiangshan.backend.execute.exu.ExuType
import xiangshan.backend.execute.fu.FuConfigs
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import xiangshan.backend.issue._
import xiangshan.backend.rename.BusyTable
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xiangshan.mem.SqPtr

class MemoryReservationStation(implicit p: Parameters) extends LazyModule{
  private val entryNum = p(XSCoreParamsKey).memRsDepth
  private val wbNodeParam = WriteBackSinkParam(name = "Memory RS", sinkType = WriteBackSinkType.memRs)
  private val rsParam = RsParam(name = "Memory RS", RsType.mem, entryNum)
  require(entryNum % rsParam.bankNum == 0)
  val issueNode = new RsIssueNode(rsParam)
  val wakeupNode = new WriteBackSinkNode(wbNodeParam)
  val dispatchNode = new RsDispatchNode(rsParam)

  lazy val module = new MemoryReservationStationImpl(this, rsParam)
}

class MemoryReservationStationImpl(outer:MemoryReservationStation, param:RsParam) extends LazyModuleImp(outer) with HasXSParameter {
  require(param.bankNum == 4)
  require(param.entriesNum % param.bankNum == 0)
  private val issue = outer.issueNode.out.head._1 zip outer.issueNode.out.head._2._2
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2._1)
  issue.foreach(elm => elm._2.exuConfigs.foreach(elm0 => require(ExuType.memTypes.contains(elm0.exuType))))

  private val staIssue = issue.filter(_._2.hasSta)
  private val stdIssue = issue.filter(_._2.hasStd)
  private val lduIssue = issue.filter(_._2.hasLoad)

  private val staIssuePortNum = issue.count(_._2.hasSta)
  private val stdIssuePortNum = issue.count(_._2.hasStd)
  private val lduIssuePortNum = issue.count(_._2.hasLoad)

  require(staIssuePortNum == stdIssuePortNum)
  require(staIssue.nonEmpty && staIssue.length <= param.bankNum && (param.bankNum % staIssue.length) == 0)
  require(stdIssue.nonEmpty && stdIssue.length <= param.bankNum && (param.bankNum % stdIssue.length) == 0)
  require(lduIssue.nonEmpty && lduIssue.length <= param.bankNum && (param.bankNum % lduIssue.length) == 0)

  private val issueWidth = issue.length
  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val specWakeup = Input(Vec(p(XSCoreParamsKey).exuParameters.specWakeUpNum, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Output(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val stLastCompelet = Input(new SqPtr)
    val integerAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val floatingAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
  })
  require(outer.dispatchNode.in.length == 1)
  private val enq = outer.dispatchNode.in.map(_._1).head

  private val wakeupSignals = VecInit(wakeup.map(_._1).map(elm =>{
    val wkp = Wire(Valid(new WakeUpInfo))
    wkp.valid := elm.valid
    wkp.bits.pdest := elm.bits.uop.pdest
    wkp.bits.robPtr := elm.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
    wkp.bits.destType := Mux(elm.bits.uop.ctrl.rfWen, SrcType.reg, Mux(elm.bits.uop.ctrl.fpWen, SrcType.fp, SrcType.default))
    wkp
  }))

  private val stIssuedWires = Wire(Vec(staIssuePortNum, Valid(new RobPtr)))

  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new MemoryReservationBank(entriesNumPerBank, staIssuePortNum, lduIssuePortNum, (wakeupSignals ++ io.specWakeup).length))
    mod.io.redirect := io.redirect
    mod.io.wakeup := wakeupSignals ++ io.specWakeup
    mod.io.loadEarlyWakeup := io.loadEarlyWakeup
    mod.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    mod.io.stIssued := stIssuedWires
    mod.io.stLastCompelet := RegNext(io.stLastCompelet)
    mod
  })
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("MemoryAllocateNetwork")))

  private val wakeupFp = wakeupSignals.zip(wakeup.map(_._2)).filter(_._2.writeFpRf).map(_._1) ++ io.specWakeup
  private val wakeupInt = wakeupSignals.zip(wakeup.map(_._2)).filter(_._2.writeIntRf).map(_._1) ++ io.specWakeup
  private val floatingBusyTable = Module(new BusyTable(param.bankNum, wakeupFp.length))
  floatingBusyTable.io.allocPregs := io.floatingAllocPregs
  floatingBusyTable.io.wbPregs.zip(wakeupFp).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.fp
    bt.bits := wb.bits.pdest
  })
  private val integerBusyTable = Module(new BusyTable(param.bankNum * 2, wakeupInt.length + io.specWakeup.length))
  integerBusyTable.io.allocPregs := io.integerAllocPregs
  integerBusyTable.io.wbPregs.zip(wakeupInt ++ io.specWakeup).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.reg
    bt.bits := wb.bits.pdest
  })

  private val staExuCfg = staIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.sta).head
  private val stdExuCfg = stdIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.std).head
  private val lduExuCfg = lduIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.ldu).head

  private val staSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, staIssuePortNum, staExuCfg, Some(s"MemoryStaSelectNetwork")))
  private val stdSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, stdIssuePortNum, stdExuCfg, Some(s"MemoryStdSelectNetwork")))
  private val lduSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, lduIssuePortNum, lduExuCfg, Some(s"MemoryLduSelectNetwork")))

  staSelectNetwork.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
    sink := source.io.staSelectInfo
  })
  staSelectNetwork.io.redirect := io.redirect

  stdSelectNetwork.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
    sink := source.io.stdSelectInfo
  })
  stdSelectNetwork.io.redirect := io.redirect

  lduSelectNetwork.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
    sink := source.io.lduSelectInfo
  })
  lduSelectNetwork.io.redirect := io.redirect

  private var fpBusyTableReadIdx = 0
  private var intBusyTableReadIdx = 0
  allocateNetwork.io.enqFromDispatch.zip(enq).foreach({case(sink, source) =>
    val fread = floatingBusyTable.io.read(fpBusyTableReadIdx)
    val iread0 = integerBusyTable.io.read(intBusyTableReadIdx)
    val iread1 = integerBusyTable.io.read(intBusyTableReadIdx + 1)
    val type0 = source.bits.ctrl.srcType(0)
    val type1 = source.bits.ctrl.srcType(1)
    fread.req := source.bits.psrc(1)
    iread0.req := source.bits.psrc(0)
    iread1.req := source.bits.psrc(1)
    sink.valid := source.valid
    sink.bits := source.bits
    sink.bits.srcState(0) := Mux(type0 === SrcType.reg, iread0.resp, SrcState.rdy)
    sink.bits.srcState(1) := Mux(type1 === SrcType.reg, iread1.resp, Mux(type1 === SrcType.fp, fread.resp, SrcState.rdy))
    source.ready := sink.ready
    fpBusyTableReadIdx = fpBusyTableReadIdx + 1
    intBusyTableReadIdx = intBusyTableReadIdx + 2
    assert(Mux(source.valid, FuType.memoryTypes.map(_ === source.bits.ctrl.fuType).reduce(_||_), true.B))
  })

  for(((fromAllocate, toAllocate), rsBank) <- allocateNetwork.io.enqToRs
    .zip(allocateNetwork.io.entriesValidBitVecList)
    .zip(rsBankSeq)){
    toAllocate := rsBank.io.allocateInfo
    rsBank.io.enq.valid := fromAllocate.valid
    rsBank.io.enq.bits.data := fromAllocate.bits.uop
    rsBank.io.enq.bits.addrOH := fromAllocate.bits.addrOH
  }

  private var earlyWkpPortIdx = 0
  private var staPortIdx = 0
  private var stdPortIdx = 0
  private var lduPortIdx = 0
  private val staIssBankNum = param.bankNum / staIssuePortNum
  private val stdIssBankNum = param.bankNum / stdIssuePortNum
  private val lduIssBankNum = param.bankNum / lduIssuePortNum
  println("\nMemory Reservation Issue Ports Config:")
  for((iss, issuePortIdx) <- issue.zipWithIndex) {
    println(s"Issue Port $issuePortIdx ${iss._2}")
    prefix(iss._2.name + "_" + iss._2.id) {
      val issueDriver = Module(new DecoupledPipeline(false, param.bankNum, entriesNumPerBank))
      issueDriver.io.redirect := io.redirect
      issueDriver.io.earlyWakeUpCancel := io.earlyWakeUpCancel

      val (finalSelectInfo, payload) = if (iss._2.isSta) {
        val res = staSelectNetwork.io.issueInfo(staPortIdx)
        val selectedBanks = rsBankSeq.slice(staPortIdx * staIssBankNum, staPortIdx * staIssBankNum + staIssBankNum)
        val bankEns = res.bits.bankIdxOH.asBools.slice(staPortIdx * staIssBankNum, staPortIdx * staIssBankNum + staIssBankNum).map(_ && res.fire)
        val bankPayloadData = selectedBanks.map(_.io.staIssueUop)
        for((b, en) <- selectedBanks.zip(bankEns)){
          b.io.staIssue.valid := en
          b.io.staIssue.bits := res.bits.entryIdxOH
        }
        val selPayload = Mux1H(bankEns, bankPayloadData)

        stIssuedWires(staPortIdx).valid := issueDriver.io.deq.fire
        stIssuedWires(staPortIdx).bits := issueDriver.io.deq.bits.uop.robIdx

        val replayPortSel = selectedBanks.map(_.io.storeReplay)
        for ((rp, idx) <- replayPortSel.zipWithIndex) {
          val fFast = iss._1.rsFeedback.feedbackFast
          val fSlow = iss._1.rsFeedback.feedbackSlow
          val faskBankIdxSel = fFast.bits.rsIdx.bankIdxOH.asBools.slice(staPortIdx * staIssBankNum, staPortIdx * staIssBankNum + staIssBankNum)
          val fastFeedBackEn = fFast.valid && faskBankIdxSel(idx)
          rp(0).valid := RegNext(fastFeedBackEn, false.B)
          rp(0).bits.entryIdxOH := RegEnable(fFast.bits.rsIdx.entryIdxOH, fastFeedBackEn)
          rp(0).bits.waitVal := RegEnable(fFast.bits.sourceType, fastFeedBackEn)

          val slowBankIdxSel = fSlow.bits.rsIdx.bankIdxOH.asBools.slice(staPortIdx * staIssBankNum, staPortIdx * staIssBankNum + staIssBankNum)
          val slowFeedBackEn = fSlow.valid && slowBankIdxSel(idx)
          rp(1).valid := RegNext(slowFeedBackEn, false.B)
          rp(1).bits.entryIdxOH := RegEnable(fSlow.bits.rsIdx.entryIdxOH, slowFeedBackEn)
          rp(1).bits.waitVal := RegEnable(fSlow.bits.sourceType, slowFeedBackEn)
        }

        staPortIdx = staPortIdx + 1
        (res, selPayload)
      } else if (iss._2.isStd) {
        val res = stdSelectNetwork.io.issueInfo(stdPortIdx)
        val selectedBanks = rsBankSeq.slice(stdPortIdx * stdIssBankNum, stdPortIdx * stdIssBankNum + stdIssBankNum)
        val bankEns = res.bits.bankIdxOH.asBools.slice(stdPortIdx * stdIssBankNum, stdPortIdx * stdIssBankNum + stdIssBankNum).map(_ && res.fire)
        val bankPayloadData = selectedBanks.map(_.io.stdIssueUop)
        for ((b, en) <- selectedBanks.zip(bankEns)) {
          b.io.stdIssue.valid := en
          b.io.stdIssue.bits := res.bits.entryIdxOH
        }
        val selPayload = Mux1H(bankEns, bankPayloadData)
        stdPortIdx = stdPortIdx + 1
        (res, selPayload)
      } else if (iss._2.isLdu) {
        val res = lduSelectNetwork.io.issueInfo(lduPortIdx)
        val selectedBanks = rsBankSeq.slice(lduPortIdx * lduIssBankNum, lduPortIdx * lduIssBankNum + lduIssBankNum)
        val bankEns = res.bits.bankIdxOH.asBools.slice(lduPortIdx * lduIssBankNum, lduPortIdx * lduIssBankNum + lduIssBankNum).map(_ && res.fire)
        val bankPayloadData = selectedBanks.map(_.io.lduIssueUop)
        for ((b, en) <- selectedBanks.zip(bankEns)) {
          b.io.lduIssue.valid := en
          b.io.lduIssue.bits := res.bits.entryIdxOH
        }
        val selPayload = Mux1H(bankEns, bankPayloadData)

        val replayPortSel = selectedBanks.map(_.io.loadReplay)
        for ((rp, idx) <- replayPortSel.zipWithIndex) {
          val fFast = iss._1.rsFeedback.feedbackFast
          val fSlow = iss._1.rsFeedback.feedbackSlow
          val faskBankIdxSel = fFast.bits.rsIdx.bankIdxOH.asBools.slice(lduPortIdx * lduIssBankNum, lduPortIdx * lduIssBankNum + lduIssBankNum)
          val fastFeedBackEn = fFast.valid && faskBankIdxSel(idx)
          rp(0).valid := RegNext(fastFeedBackEn, false.B)
          rp(0).bits.entryIdxOH := RegEnable(fFast.bits.rsIdx.entryIdxOH, fastFeedBackEn)
          rp(0).bits.waitVal := RegEnable(fFast.bits.sourceType, fastFeedBackEn)

          val slowBankIdxSel = fSlow.bits.rsIdx.bankIdxOH.asBools.slice(lduPortIdx * lduIssBankNum, lduPortIdx * lduIssBankNum + lduIssBankNum)
          val slowFeedBackEn = fSlow.valid && slowBankIdxSel(idx)
          rp(1).valid := RegNext(slowFeedBackEn, false.B)
          rp(1).bits.entryIdxOH := RegEnable(fSlow.bits.rsIdx.entryIdxOH, slowFeedBackEn)
          rp(1).bits.waitVal := RegEnable(fSlow.bits.sourceType, slowFeedBackEn)
        }

        val earlyWakeupQueue = Module(new WakeupQueue(3))
        earlyWakeupQueue.io.in.valid := res.fire
        earlyWakeupQueue.io.earlyWakeUpCancel := io.earlyWakeUpCancel
        earlyWakeupQueue.io.in.bits.robPtr := res.bits.info.robPtr
        earlyWakeupQueue.io.in.bits.lpv := res.bits.info.lpv
        earlyWakeupQueue.io.in.bits.pdest := res.bits.info.pdest
        earlyWakeupQueue.io.in.bits.destType := Mux(res.bits.info.fpWen, SrcType.fp, Mux(res.bits.info.rfWen, SrcType.reg, SrcType.default))
        earlyWakeupQueue.io.redirect := io.redirect

        earlyWakeupQueue.io.in.bits.lpv(lduPortIdx) := res.bits.info.lpv(lduPortIdx) | (1 << (LpvLength - 1)).U
        io.loadEarlyWakeup(earlyWkpPortIdx).valid := earlyWakeupQueue.io.out.valid
        io.loadEarlyWakeup(earlyWkpPortIdx).bits.robPtr := earlyWakeupQueue.io.out.bits.robPtr
        io.loadEarlyWakeup(earlyWkpPortIdx).bits.pdest := earlyWakeupQueue.io.out.bits.pdest
        io.loadEarlyWakeup(earlyWkpPortIdx).bits.destType := earlyWakeupQueue.io.out.bits.destType
        io.loadEarlyWakeup(earlyWkpPortIdx).bits.lpv := earlyWakeupQueue.io.out.bits.lpv(lduPortIdx)

        earlyWkpPortIdx = earlyWkpPortIdx + 1
        lduPortIdx = lduPortIdx + 1
        (res, selPayload)
      } else {
        require(false, "Unknown Exu complex!")
        val res = Wire(Decoupled(new SelectResp(param.bankNum, entriesNumPerBank)))
        val payload = Wire(new MicroOp)
        res := DontCare
        payload := DontCare
        (res, payload)
      }

      val issueBundle = Wire(Valid(new MicroOp))
      issueBundle.valid := finalSelectInfo.valid
      issueBundle.bits := payload
      issueBundle.bits.robIdx := finalSelectInfo.bits.info.robPtr
      issueBundle.bits.ctrl.rfWen := finalSelectInfo.bits.info.rfWen
      issueBundle.bits.ctrl.fpWen := finalSelectInfo.bits.info.fpWen
      issueBundle.bits.pdest := finalSelectInfo.bits.info.pdest
      issueBundle.bits.ctrl.fuType := finalSelectInfo.bits.info.fuType
      issueBundle.bits.lpv := finalSelectInfo.bits.info.lpv

      finalSelectInfo.ready := issueDriver.io.enq.ready
      issueDriver.io.enq.valid := issueBundle.valid
      issueDriver.io.enq.bits.uop := issueBundle.bits
      issueDriver.io.enq.bits.bankIdxOH := finalSelectInfo.bits.bankIdxOH
      issueDriver.io.enq.bits.entryIdxOH := finalSelectInfo.bits.entryIdxOH

      iss._1.issue.valid := issueDriver.io.deq.valid
      iss._1.issue.bits.uop := issueDriver.io.deq.bits.uop
      iss._1.issue.bits.src := DontCare
      iss._1.rsIdx.bankIdxOH := issueDriver.io.deq.bits.bankIdxOH
      iss._1.rsIdx.entryIdxOH := issueDriver.io.deq.bits.entryIdxOH
      issueDriver.io.deq.ready := iss._1.issue.ready
    }
  }
  println("\nMemory Reservation Wake Up Ports Config:")
  wakeup.zipWithIndex.foreach({ case ((_, cfg), idx) =>
    println(s"Wake Port $idx ${cfg.name} of ${cfg.complexName} #${cfg.id}")
  })
}

