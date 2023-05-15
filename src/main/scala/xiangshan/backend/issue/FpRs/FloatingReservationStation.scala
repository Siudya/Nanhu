package xiangshan.backend.issue.FpRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import xiangshan.backend.execute.exu.ExuType
import xiangshan.backend.execute.fu.fpu.FMAMidResult
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.issue._
import xiangshan.backend.writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xiangshan._
import xiangshan.backend.execute.fu.fpu.FMAMidResult
import xs.utils.Assertion.xs_assert

class FloatingReservationStation(implicit p: Parameters) extends LazyModule with HasXSParameter {
  private val entryNum = p(XSCoreParamsKey).fpRsDepth
  private val wbNodeParam = WriteBackSinkParam(name = "Floating RS", sinkType = WriteBackSinkType.fpRs)
  private val rsParam = RsParam(name = "Floating RS", RsType.fp, entryNum)
  require(entryNum % rsParam.bankNum == 0)
  val issueNode = new RsIssueNode(rsParam)
  val wakeupNode = new WriteBackSinkNode(wbNodeParam)
  val dispatchNode = new RsDispatchNode(rsParam)

  lazy val module = new FloatingReservationStationImpl(this, rsParam)
}

class FloatingReservationStationImpl(outer:FloatingReservationStation, param:RsParam)(implicit p: Parameters) extends LazyModuleImp(outer) with HasXSParameter {
  require(param.bankNum == 4)
  require(param.entriesNum % param.bankNum == 0)
  private val issue = outer.issueNode.out.head._1 zip outer.issueNode.out.head._2._2
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2._1)
  issue.foreach(elm => elm._2.exuConfigs.foreach(elm0 => require(ExuType.fpTypes.contains(elm0.exuType))))

  private val fmacIssue = issue.filter(_._2.hasFmac)
  private val fdivIssue = issue.filter(_._2.hasFdiv)
  private val fmiscIssue = issue.filter(_._2.hasFmisc)

  require(fmacIssue.nonEmpty && fmacIssue.length <= param.bankNum && (param.bankNum % fmacIssue.length) == 0)
  require(fdivIssue.nonEmpty && fdivIssue.length <= param.bankNum && (param.bankNum % fdivIssue.length) == 0)
  require(fmiscIssue.nonEmpty && fmiscIssue.length <= param.bankNum && (param.bankNum % fmiscIssue.length) == 0)

  private val issueWidth = issue.length
  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val loadEarlyWakeup = Input(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })
  require(outer.dispatchNode.in.length == 1)
  private val enq = outer.dispatchNode.in.map(_._1).head

  private val wakeupSignals = VecInit(wakeup.map(_._1).map(elm =>{
    val wkp = Wire(Valid(new WakeUpInfo))
    wkp.valid := elm.valid
    wkp.bits.pdest := elm.bits.uop.pdest
    wkp.bits.robPtr := elm.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
    wkp.bits.destType := Mux(elm.bits.uop.ctrl.fpWen, SrcType.fp, SrcType.default)
    wkp
  }))
  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new FloatingReservationBank(entriesNumPerBank, issueWidth, wakeup.length, loadUnitNum))
    mod.io.redirect := io.redirect
    mod.io.wakeup := wakeupSignals
    mod.io.loadEarlyWakeup := io.loadEarlyWakeup
    mod.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    mod
  })
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("IntegerAllocateNetwork")))

  private val fmaIssuePortNum = issue.count(_._2.hasFmac)
  private val fdivIssuePortNum = issue.count(_._2.hasFdiv)
  private val fmiscIssuePortNum = issue.count(_._2.hasFmisc)
  private val fmaExuCfg = fmacIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.fmac).head
  private val fdivExuCfg = fdivIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.fdiv).head
  private val fmiscExuCfg = fmiscIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.fmisc).head

  private val fmacSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, fmaIssuePortNum, fmaExuCfg, Some(s"FloatingFmacSelectNetwork")))
  private val fdivSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, fdivIssuePortNum, fdivExuCfg, Some(s"FloatingFdivSelectNetwork")))
  private val fmiscSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, fmiscIssuePortNum, fmiscExuCfg, Some(s"FloatingFmiscSelectNetwork")))
  fdivSelectNetwork.io.tokenRelease.get.zip(wakeup.filter(_._2.exuType == ExuType.fdiv).map(_._1)).foreach({
    case(sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits.uop.pdest
  })
  private val selectNetworkSeq = Seq(fmacSelectNetwork, fdivSelectNetwork, fmiscSelectNetwork)
  selectNetworkSeq.foreach(sn => {
    sn.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
      sink := source.io.selectInfo
    })
    sn.io.redirect := io.redirect
  })

  allocateNetwork.io.enqFromDispatch.zip(enq).foreach({case(sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
    source.ready := sink.ready
    xs_assert(Mux(source.valid, FuType.floatingTypes.map(_ === source.bits.ctrl.fuType).reduce(_||_), true.B))
  })

  for(((fromAllocate, toAllocate), rsBank) <- allocateNetwork.io.enqToRs
    .zip(allocateNetwork.io.entriesValidBitVecList)
    .zip(rsBankSeq)){
    toAllocate := rsBank.io.allocateInfo
    rsBank.io.enq.valid := fromAllocate.valid
    rsBank.io.enq.bits.data := fromAllocate.bits.uop
    rsBank.io.enq.bits.addrOH := fromAllocate.bits.addrOH
  }

  private var fmaPortIdx = 0
  private var fdivPortIdx = 0
  private var fmiscPortIdx = 0
  println("\nFloating Point Reservation Issue Ports Config:")
  for((iss, issuePortIdx) <- issue.zipWithIndex) {
    println(s"Issue Port $issuePortIdx ${iss._2}")
    prefix(iss._2.name + "_" + iss._2.id) {
      val issueDriver = Module(new DecoupledPipeline(true, param.bankNum, entriesNumPerBank))
      issueDriver.io.redirect := io.redirect
      issueDriver.io.earlyWakeUpCancel := io.earlyWakeUpCancel

      val midStateWaitQueue = Module(new MidStateWaitQueue(3, param.bankNum, entriesNumPerBank))
      midStateWaitQueue.io.redirect := io.redirect

      val finalSelectInfo = if (iss._2.isFmac) {
        fmaPortIdx = fmaPortIdx + 1
        fmacSelectNetwork.io.issueInfo(fmaPortIdx - 1)
      } else if (iss._2.isFmaDiv) {
        fmaPortIdx = fmaPortIdx + 1
        fdivPortIdx = fdivPortIdx + 1
        val selectRespArbiter = Module(new SelectRespArbiter(param.bankNum, entriesNumPerBank, 2))
        selectRespArbiter.io.in(0) <> fmacSelectNetwork.io.issueInfo(fmaPortIdx - 1)
        selectRespArbiter.io.in(1) <> fdivSelectNetwork.io.issueInfo(fdivPortIdx - 1)
        selectRespArbiter.io.out
      } else {
        fmaPortIdx = fmaPortIdx + 1
        fmiscPortIdx = fmiscPortIdx + 1
        val selectRespArbiter = Module(new SelectRespArbiter(param.bankNum, entriesNumPerBank, 2))
        selectRespArbiter.io.in(0) <> fmacSelectNetwork.io.issueInfo(fmaPortIdx - 1)
        selectRespArbiter.io.in(1) <> fmiscSelectNetwork.io.issueInfo(fmiscPortIdx - 1)
        selectRespArbiter.io.out
      }

      val rsBankRen = Mux(issueDriver.io.enq.fire, finalSelectInfo.bits.bankIdxOH, 0.U)
      rsBankSeq.zip(rsBankRen.asBools).foreach({ case (rb, ren) =>
        rb.io.issueAddr(issuePortIdx).valid := ren
        rb.io.issueAddr(issuePortIdx).bits := finalSelectInfo.bits.entryIdxOH
      })

      val issueBundle = Wire(Valid(new MicroOp))
      issueBundle.valid := finalSelectInfo.valid
      issueBundle.bits := Mux1H(rsBankRen, rsBankSeq.map(_.io.issueUop(issuePortIdx).bits))
      issueBundle.bits.robIdx := finalSelectInfo.bits.info.robPtr
      issueBundle.bits.ctrl.rfWen := finalSelectInfo.bits.info.rfWen
      issueBundle.bits.ctrl.fpWen := finalSelectInfo.bits.info.fpWen
      issueBundle.bits.pdest := finalSelectInfo.bits.info.pdest
      issueBundle.bits.ctrl.fuType := finalSelectInfo.bits.info.fuType
      issueBundle.bits.lpv := finalSelectInfo.bits.info.lpv

      val midResultFromPayload = Wire(new FMAMidResult)
      val midResultFromPayloadHi = Mux1H(rsBankRen, rsBankSeq.map(_.io.issueMidResult(issuePortIdx)))
      val midResultFromPayloadLo = 0.U(XLEN - 1, 0)
      midResultFromPayload := Cat(midResultFromPayloadHi, midResultFromPayloadLo).asTypeOf(midResultFromPayload)

      val midResultFromBypass = Wire(new FMAMidResult)
      midResultFromBypass := iss._1.fmaMidState.out.bits.midResult

      val midResult = Wire(new FMAMidResult)

      finalSelectInfo.ready := issueDriver.io.enq.ready
      issueDriver.io.enq.valid := issueBundle.valid
      issueDriver.io.enq.bits.uop := issueBundle.bits
      issueDriver.io.enq.bits.fmaMidStateIssue.valid := finalSelectInfo.bits.info.midResultReadEn
      issueDriver.io.enq.bits.fmaMidStateIssue.bits := midResult
      issueDriver.io.enq.bits.fmaWaitForAdd := finalSelectInfo.bits.info.fmaWaitAdd
      issueDriver.io.enq.bits.bankIdxOH := finalSelectInfo.bits.bankIdxOH
      issueDriver.io.enq.bits.entryIdxOH := finalSelectInfo.bits.entryIdxOH

      iss._1.issue.valid := issueDriver.io.deq.valid
      iss._1.issue.bits.uop := issueDriver.io.deq.bits.uop
      iss._1.issue.bits.src := DontCare
      iss._1.fmaMidState.in := issueDriver.io.deq.bits.fmaMidStateIssue
      iss._1.fmaMidState.waitForAdd := issueDriver.io.deq.bits.fmaWaitForAdd
      iss._1.rsIdx.bankIdxOH := issueDriver.io.deq.bits.bankIdxOH
      iss._1.rsIdx.entryIdxOH := issueDriver.io.deq.bits.entryIdxOH
      issueDriver.io.deq.ready := iss._1.issue.ready

      val midStateWaitQueueInValidReg = RegInit(false.B)
      val midStateWaitQueueInDataReg = Reg(new SelectResp(param.bankNum, entriesNumPerBank))
      when(issueDriver.io.deq.fire) {
        midStateWaitQueueInValidReg := issueDriver.io.deq.bits.fmaWaitForAdd
      }
      when(issueDriver.io.deq.fire && issueDriver.io.deq.bits.fmaWaitForAdd) {
        midStateWaitQueueInDataReg.bankIdxOH := issueDriver.io.deq.bits.bankIdxOH
        midStateWaitQueueInDataReg.entryIdxOH := issueDriver.io.deq.bits.entryIdxOH
        midStateWaitQueueInDataReg.info := DontCare
        midStateWaitQueueInDataReg.info.robPtr := issueDriver.io.deq.bits.uop.robIdx
        midStateWaitQueueInDataReg.info.pdest := issueDriver.io.deq.bits.uop.pdest
      }
      midStateWaitQueue.io.in.valid := midStateWaitQueueInValidReg && iss._1.fuInFire && !midStateWaitQueueInDataReg.info.robPtr.needFlush(io.redirect)
      midStateWaitQueue.io.in.bits := midStateWaitQueueInDataReg

      val bankForThisWaitQueue = rsBankSeq(issuePortIdx)
      val bankEn = Mux(midStateWaitQueue.io.earlyWakeUp.valid, midStateWaitQueue.io.out.bits.bankIdxOH, 0.U)
        .asBools(issuePortIdx)
      bankForThisWaitQueue.io.midResultReceived.valid := bankEn
      bankForThisWaitQueue.io.midResultReceived.bits := midStateWaitQueue.io.earlyWakeUp.bits.entryIdxOH

      val midStateShouldBypass =
        iss._1.fmaMidState.out.valid &&
          midStateWaitQueue.io.out.valid &&
          midStateWaitQueue.io.out.bits.info.pdest === iss._1.fmaMidState.out.bits.pdest

      midResult := Mux(midStateShouldBypass, midResultFromBypass, midResultFromPayload)

      val midResultWidth = iss._1.fmaMidState.out.bits.midResult.getWidth
      bankForThisWaitQueue.io.midResultEnq.valid := midStateWaitQueue.io.out.valid
      bankForThisWaitQueue.io.midResultEnq.bits.addrOH := midStateWaitQueue.io.out.bits.entryIdxOH
      bankForThisWaitQueue.io.midResultEnq.bits.data := iss._1.fmaMidState.out.bits.midResult.asUInt(midResultWidth - 1, XLEN)
    }
  }
  println("\nFloating Reservation Wake Up Ports Config:")
  wakeup.zipWithIndex.foreach({ case ((_, cfg), idx) =>
    println(s"Wake Port $idx ${cfg.name} of ${cfg.complexName} #${cfg.id}")
  })
}
