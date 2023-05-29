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
import xiangshan.backend.rename.BusyTable
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
  private val wakeupWidth = wakeupSignals.length
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("FloatingAllocateNetwork")))
  private val floatingBusyTable = Module(new BusyTable(param.bankNum * 3, wakeupWidth))
  floatingBusyTable.io.allocPregs := io.floatingAllocPregs
  floatingBusyTable.io.wbPregs.zip(wakeupSignals).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.fp
    bt.bits := wb.bits.pdest
  })

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

  private var busyTableReadIdx = 0
  allocateNetwork.io.enqFromDispatch.zip(enq).foreach({case(sink, source) =>
    val rport0 = floatingBusyTable.io.read(busyTableReadIdx)
    val rport1 = floatingBusyTable.io.read(busyTableReadIdx + 1)
    val rport2 = floatingBusyTable.io.read(busyTableReadIdx + 2)
    rport0.req := source.bits.psrc(0)
    rport1.req := source.bits.psrc(1)
    rport2.req := source.bits.psrc(2)
    sink.valid := source.valid
    sink.bits := source.bits
    sink.bits.srcState(0) := Mux(source.bits.ctrl.srcType(0) === SrcType.fp, rport0.resp, SrcState.rdy)
    sink.bits.srcState(1) := Mux(source.bits.ctrl.srcType(1) === SrcType.fp, rport1.resp, SrcState.rdy)
    sink.bits.srcState(2) := Mux(source.bits.ctrl.srcType(2) === SrcType.fp, rport2.resp, SrcState.rdy)
    source.ready := sink.ready
    busyTableReadIdx = busyTableReadIdx + 3
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
  println("\nFloating Reservation Wake Up Ports Config:")
  wakeup.zipWithIndex.foreach({ case ((_, cfg), idx) =>
    println(s"Wake Port $idx ${cfg.name} of ${cfg.complexName} #${cfg.id}")
  })
}
