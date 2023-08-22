/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package xiangshan.backend.execute.exublock

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import coupledL2.PrefetchRecv
import coupledL2.prefetch.{PrefetchReceiverParams}
import utils._
import xiangshan._
import xiangshan.backend.execute.exu.{ExuConfig, ExuInputNode, ExuOutputNode, ExuType}
import xiangshan.backend.execute.exucx.ExuComplexIssueNode
import xiangshan.backend.execute.fu.{FuConfigs, FunctionUnit, PMP, PMPChecker, PMPCheckerv2}
import xiangshan.backend.execute.fu.csr.{PFEvent, SdtrigExt}
import xiangshan.backend.execute.fu.fence.{FenceToSbuffer, SfenceBundle}
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.cache.mmu.{BTlbPtwIO, TLB, TlbIO, TlbReplace}
import xiangshan.mem._
import xiangshan.mem.prefetch.{BasePrefecher, SMSParams, SMSPrefetcher}
import xs.utils.mbist.MBISTPipeline
import xs.utils.{DelayN, DelayNWithCG, ParallelPriorityMux, RegNextN, RegNextNWithCG, RegNextWithCG, ValidIODelay}

class Std(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new ExuInput))
    val out = DecoupledIO(new ExuOutput)
    val redirect = Input(Valid(new Redirect))
  })
  private val validReg = RegNext(io.in.valid, false.B)
  private val bitsReg = RegEnable(io.in.bits, io.in.valid)
  io.in.ready := true.B
  io.out.valid := validReg && !bitsReg.uop.robIdx.needFlush(io.redirect)
  io.out.bits := DontCare
  io.out.bits.uop := bitsReg.uop
  io.out.bits.data := bitsReg.src(0)
}
class MemIssueRouter(implicit p: Parameters) extends LazyModule{
  val node = new ExuComplexIssueNode
  lazy val module = new LazyModuleImp(this){
    require(node.in.length == 1)
    private val ib = node.in.head._1
    for((ob,oe) <- node.out) {
      ob.issue.valid := ib.issue.valid && ib.issue.bits.uop.ctrl.fuType === oe._2.fuConfigs.head.fuType
      ob.issue.bits := ib.issue.bits
      ib.issue.ready := true.B
      assert(ob.issue.ready === true.B)
      ob.rsIdx := ib.rsIdx
      ob.auxValid := ib.auxValid
      if (oe._2.fuConfigs.head.name == "ldu") {
        ib.rsFeedback.feedbackFastLoad := ob.rsFeedback.feedbackFastLoad
        ib.rsFeedback.feedbackSlowLoad := ob.rsFeedback.feedbackSlowLoad
      } else if (oe._2.fuConfigs.head.name == "sta") {
        ib.rsFeedback.feedbackSlowStore := ob.rsFeedback.feedbackSlowStore
      }
    }
  }
}

class MemBlock(val parentName:String = "Unknown")(implicit p: Parameters) extends BasicExuBlock
  with HasXSParameter{

  private val lduParams = Seq.tabulate(exuParameters.LduCnt)(idx => {
    ExuConfig(
      name = "LduExu",
      id = idx,
      complexName = "MemComplex",
      fuConfigs = Seq(FuConfigs.lduCfg),
      exuType = ExuType.ldu
    )
  })
  private val staParams = Seq.tabulate(exuParameters.StuCnt)(idx => {
    ExuConfig(
      name = "StaExu",
      id = idx,
      complexName = "MemComplex",
      fuConfigs = Seq(FuConfigs.staCfg),
      exuType = ExuType.sta
    )
  })
  private val stdParams = Seq.tabulate(exuParameters.StuCnt)(idx => {
    ExuConfig(
      name = "StdExu",
      id = idx,
      complexName = "MemComplex",
      fuConfigs = Seq(FuConfigs.stdCfg),
      exuType = ExuType.std
    )
  })
  private val specialLduParams = ExuConfig(
    name = "SpecialLduExu",
    id = 0,
    complexName = "MemComplex",
    fuConfigs = Seq(FuConfigs.specialLduCfg),
    exuType = ExuType.sldu
  )
  val lduIssueNodes: Seq[ExuInputNode] = lduParams.zipWithIndex.map(e => new ExuInputNode(e._1))
  val specialLduIssueNode: MemoryBlockIssueNode = new MemoryBlockIssueNode((specialLduParams,0))
  val lduWritebackNodes: Seq[ExuOutputNode] = lduParams.map(new ExuOutputNode(_))
  val staIssueNodes: Seq[ExuInputNode] = staParams.zipWithIndex.map(e => new ExuInputNode(e._1))
  val staWritebackNodes: Seq[ExuOutputNode] = staParams.map(new ExuOutputNode(_))
  val stdIssueNodes: Seq[ExuInputNode] = stdParams.zipWithIndex.map(e => new ExuInputNode(e._1))
  val stdWritebackNodes: Seq[ExuOutputNode] = stdParams.map(new ExuOutputNode(_))

  val memIssueRouters: Seq[MemIssueRouter] = Seq.fill(2)(LazyModule(new MemIssueRouter))
  memIssueRouters.zip(lduIssueNodes).zip(staIssueNodes).zip(stdIssueNodes).foreach({case(((mir, ldu), sta), std) =>
    ldu :*= mir.node
    sta :*= mir.node
    std :*= mir.node
  })

  private val allWritebackNodes = lduWritebackNodes ++ staWritebackNodes ++ stdWritebackNodes

  memIssueRouters.foreach(mir => mir.node :*= issueNode)
  specialLduIssueNode :*= issueNode
  allWritebackNodes.foreach(onode => writebackNode :=* onode)

  val dcache = LazyModule(new DCacheWrapper(parentName = parentName + "dcache_"))
  val uncache = LazyModule(new Uncache())
  val pf_sender_opt = coreParams.prefetcher match  {
    case Some(receive : SMSParams) => Some(BundleBridgeSource(() => new PrefetchRecv))
    // case sms_sender_hyper : HyperPrefetchParams => Some(BundleBridgeSource(() => new PrefetchRecv))
    case _ => None
  }

  lazy val module = new MemBlockImp(this)

}

class MemBlockImp(outer: MemBlock) extends BasicExuBlockImp(outer)
  with HasXSParameter
  with HasFPUParameters
  with HasPerfEvents
  with SdtrigExt
{
  private val lduIssues = outer.lduIssueNodes.map(iss => {
    require(iss.in.length == 1)
    iss.in.head._1
  })
  private val sludIssue = outer.specialLduIssueNode.in.head._1
  require(outer.specialLduIssueNode.in.length == 1)
  private val staIssues = outer.staIssueNodes.map(iss => {
    require(iss.in.length == 1)
    iss.in.head._1
  })
  private val stdIssues = outer.stdIssueNodes.map(iss => {
    require(iss.in.length == 1)
    iss.in.head._1
  })
  private val lduWritebacks = outer.lduWritebackNodes.map(wb => {
    require(wb.out.length == 1)
    wb.out.head._1
  })
  private val staWritebacks = outer.staWritebackNodes.map(wb => {
    require(wb.out.length == 1)
    wb.out.head._1
  })
  private val stdWritebacks = outer.stdWritebackNodes.map(wb => {
    require(wb.out.length == 1)
    wb.out.head._1
  })

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    // in
    val stIssuePtr = Output(new SqPtr())
    // out
    val s3_delayed_load_error = Vec(exuParameters.LduCnt, Output(Bool()))
    // misc
    val stIn = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
    val ptw = new BTlbPtwIO(ld_tlb_ports + exuParameters.StuCnt)
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val fenceToSbuffer = Flipped(new FenceToSbuffer)
    val enqLsq = new LsqEnqIO
    // val memPredUpdate = Vec(exuParameters.StuCnt, Input(new MemPredUpdateReq))
    val lsqio = new Bundle {
      val exceptionAddr = new ExceptionAddrIO // to csr
      val rob = Flipped(new RobLsqIO) // rob to lsq
    }
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val csrUpdate = new DistributedCSRUpdateReq
    val error = new L1CacheErrorInfo
    val memInfo = new Bundle {
      val sqFull = Output(Bool())
      val lqFull = Output(Bool())
      val dcacheMSHRFull = Output(Bool())
    }

    val earlyWakeUpCancel = Output(Vec(3, Vec(lduIssues.length, Bool())))
    val issueToMou = Flipped(Decoupled(new ExuInput))
    val writebackFromMou = Decoupled(new ExuOutput)

    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
    val perfEventsPTW = Input(Vec(19, new PerfEvent))
    val lqCancelCnt = Output(UInt(log2Up(LoadQueueSize + 1).W))
    val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
    val sqDeq = Output(UInt(2.W))
  })

  val dcache = outer.dcache.module
  val uncache = outer.uncache.module

  val csrCtrl = DelayNWithCG(io.csrCtrl, 2)
  dcache.io.csr.distribute_csr <> csrCtrl.distribute_csr
  dcache.io.l2_pf_store_only := RegNext(io.csrCtrl.l2_pf_store_only)
  io.csrUpdate := RegNext(dcache.io.csr.update)
  io.error <> RegNext(RegNext(dcache.io.error))
  when(!csrCtrl.cache_error_enable){
    io.error.report_to_beu := false.B
    io.error.valid := false.B
  }

  private val loadUnits = Seq.fill(exuParameters.LduCnt)(Module(new LoadUnit))
  private val storeUnits = Seq.fill(exuParameters.StuCnt)(Module(new StoreUnit))
  private val stdUnits = Seq.fill(exuParameters.StuCnt)(Module(new Std))
  private val stData = stdUnits.map(_.io.out)
  val prefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher match {
    case Some(sms_sender: SMSParams) =>
      val sms = Module(new SMSPrefetcher(parentName = outer.parentName + "sms_"))
      sms.io_agt_en := RegNextN(io.csrCtrl.l1D_pf_enable_agt, 2, Some(false.B))
      sms.io_pht_en := RegNextN(io.csrCtrl.l1D_pf_enable_pht, 2, Some(false.B))
      sms.io_act_threshold := RegNextN(io.csrCtrl.l1D_pf_active_threshold, 2, Some(12.U))
      sms.io_act_stride := RegNextN(io.csrCtrl.l1D_pf_active_stride, 2, Some(30.U))
      sms.io_stride_en := RegNextN(io.csrCtrl.l1D_pf_enable_stride, 2, Some(true.B))
      Some(sms)
    // case Some(sms_sender_hyper : HyperPrefetchParams) =>
    //   val sms = Module(new SMSPrefetcher(parentName = outer.parentName + "sms_"))
    //   sms.io_agt_en := RegNextN(io.csrCtrl.l1D_pf_enable_agt, 2, Some(false.B))
    //   sms.io_pht_en := RegNextN(io.csrCtrl.l1D_pf_enable_pht, 2, Some(false.B))
    //   sms.io_act_threshold := RegNextN(io.csrCtrl.l1D_pf_active_threshold, 2, Some(12.U))
    //   sms.io_act_stride := RegNextN(io.csrCtrl.l1D_pf_active_stride, 2, Some(30.U))
    //   sms.io_stride_en := RegNextN(io.csrCtrl.l1D_pf_enable_stride, 2, Some(true.B))
    //   Some(sms)
    case _ => None
  }
  prefetcherOpt match {
    case Some(sms) => // memblock can only have sms or not
      outer.pf_sender_opt match{
        case Some(sender) =>
        val pf_to_l2 = Pipe(sms.io.pf_addr, 2)
        sender.out.head._1.addr_valid := pf_to_l2.valid
        sender.out.head._1.addr := pf_to_l2.bits
        sender.out.head._1.l2_pf_en := RegNextN(io.csrCtrl.l2_pf_enable, 2, Some(true.B))
        sms.io.enable := RegNextN(io.csrCtrl.l1D_pf_enable, 2, Some(false.B))
        case None => assert(cond = false, "Maybe from Config fault: Open SMS but dont have sms sender&recevier")
      }
    case None =>
  }
  private val pf_train_on_hit = RegNextN(io.csrCtrl.l1D_pf_train_on_hit, 2, Some(true.B))

  loadUnits.zipWithIndex.map(x => x._1.suggestName("LoadUnit_"+x._2))
  storeUnits.zipWithIndex.map(x => x._1.suggestName("StoreUnit_"+x._2))

  private val atomicsUnit = Module(new AtomicsUnit)

  io.writebackFromMou <> atomicsUnit.io.out
  private val ldExeWbReqs = loadUnits.map(_.io.ldout)
  private val staExeWbReqs = storeUnits.map(_.io.stout)
  private val stdExeWbReqs = stdUnits.map(_.io.out)
  (lduWritebacks ++ staWritebacks)
    .zip(ldExeWbReqs ++ staExeWbReqs)
    .foreach({case(wb, out) =>
      wb.valid := out.valid
      wb.bits := out.bits
      out.ready := true.B
  })
  stdWritebacks.zip(stdExeWbReqs)
    .foreach({ case (wb, out) =>
      wb.valid := out.valid
      wb.bits := out.bits
      out.ready := true.B
    })
  lduWritebacks.zip(ldExeWbReqs).foreach({case(wb, out) =>
    val redirect_wb = wb.bits.redirect
    val uop_out = out.bits.uop
    wb.bits.redirectValid := out.fire && out.bits.uop.ctrl.replayInst
    redirect_wb.robIdx := uop_out.robIdx
    redirect_wb.ftqIdx := uop_out.cf.ftqPtr
    redirect_wb.ftqOffset := uop_out.cf.ftqOffset
    redirect_wb.level := RedirectLevel.flush
    redirect_wb.interrupt := false.B
    redirect_wb.cfiUpdate := DontCare
    redirect_wb.cfiUpdate.isMisPred := false.B
    redirect_wb.isException := false.B
    redirect_wb.isLoadStore := false.B
    redirect_wb.isLoadLoad := true.B
    redirect_wb.isXRet := false.B
    redirect_wb.isFlushPipe := false.B
  })

  private val stOut = staWritebacks

  // TODO: fast load wakeup
  val lsq     = Module(new LsqWrappper)
  val sbuffer = Module(new Sbuffer)
  // if you wants to stress test dcache store, use FakeSbuffer
  // val sbuffer = Module(new FakeSbuffer)
  io.stIssuePtr := lsq.io.issuePtrExt

  dcache.io.hartId := io.hartId
  lsq.io.hartId := io.hartId
  sbuffer.io.hartId := io.hartId
  atomicsUnit.io.hartId := io.hartId

  // dtlb
  val total_tlb_ports = ld_tlb_ports + exuParameters.StuCnt
  val NUMSfenceDup = 3
  val NUMTlbCsrDup = 8
  val sfence_dup = Seq.fill(NUMSfenceDup)(Pipe(io.sfence))
  val tlbcsr_dup = Seq.fill(NUMTlbCsrDup)(RegNext(io.tlbCsr))


  val dtlb_ld_st = VecInit(Seq.fill(1) {
    val dtlb = Module(new TLB(ld_tlb_ports + exuParameters.StuCnt, 2, OnedtlbParams))
    dtlb.io
  })
  if(!UseOneDtlb){
    dtlb_ld_st := DontCare
  }

  val dtlb_ld : Seq[TlbIO] = if(UseOneDtlb) {
    dtlb_ld_st.take(ld_tlb_ports)
  } else {
    VecInit(Seq.fill(1) {
      val tlb_ld =  Module(new TLB(ld_tlb_ports, 2, ldtlbParams))
      tlb_ld.io // let the module have name in waveform
    })
  }

  val dtlb_st : Seq[TlbIO] = if(UseOneDtlb){
    dtlb_ld_st.drop(ld_tlb_ports)
  } else {
      VecInit(Seq.fill(1) {
      val tlb_st = Module(new TLB(exuParameters.StuCnt, 1, sttlbParams))
      tlb_st.io // let the module have name in waveform
    })
  }

  val dtlb = dtlb_ld ++ dtlb_st
  val dtlb_reqs = dtlb.flatMap(_.requestor)
  val dtlb_pmps = dtlb.flatMap(_.pmp)
  dtlb.zip(sfence_dup.take(2)).foreach{ case (d,s) => d.sfence := s }
  dtlb.zip(tlbcsr_dup.take(2)).foreach{ case (d,c) => d.csr := c }
  if (refillBothTlb) {
    require(ldtlbParams.outReplace == sttlbParams.outReplace)
    require(ldtlbParams.outReplace)

    val replace = Module(new TlbReplace(total_tlb_ports, ldtlbParams))
    replace.io.apply_sep(dtlb_ld.map(_.replace) ++ dtlb_st.map(_.replace), io.ptw.resp.bits.data.entry.tag)
  } else {
    if(!UseOneDtlb){
      if (ldtlbParams.outReplace) {
        val replace_ld = Module(new TlbReplace(ld_tlb_ports, ldtlbParams))
        replace_ld.io.apply_sep(dtlb_ld.map(_.replace), io.ptw.resp.bits.data.entry.tag)
      }
      if (sttlbParams.outReplace) {
        val replace_st = Module(new TlbReplace(exuParameters.StuCnt, sttlbParams))
        replace_st.io.apply_sep(dtlb_st.map(_.replace), io.ptw.resp.bits.data.entry.tag)
      }
    }
  }

  val ptw_resp_next = RegEnable(io.ptw.resp.bits, io.ptw.resp.valid)
  val ptw_resp_v = RegNext(
    io.ptw.resp.valid && !(RegNext(sfence_dup.last.valid && tlbcsr_dup.last.satp.changed)),
    init = false.B
  )
  io.ptw.resp.ready := true.B

  dtlb.flatMap(a => a.ptw.req)
    .zipWithIndex
    .foreach{ case (tlb, i) =>
    tlb <> io.ptw.req(i)
    val vector_hit = if (refillBothTlb) Cat(ptw_resp_next.vector).orR
      else if (i < ld_tlb_ports) Cat(ptw_resp_next.vector.take(ld_tlb_ports)).orR
      else Cat(ptw_resp_next.vector.drop(ld_tlb_ports)).orR
    io.ptw.req(i).valid := tlb.valid && !(ptw_resp_v && vector_hit &&
      ptw_resp_next.data.entry.hit(tlb.bits.vpn, RegNext(tlbcsr_dup(i).satp.asid), allType = true, ignoreAsid = true))
  }
  dtlb.foreach(_.ptw.resp.bits := ptw_resp_next.data)
  if (refillBothTlb || UseOneDtlb) {
    dtlb.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR)
  } else {
    dtlb_ld.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.take(ld_tlb_ports)).orR)
    dtlb_st.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.drop(ld_tlb_ports)).orR)
  }


  // pmp
  val pmp = Module(new PMP())
  pmp.io.distribute_csr <> csrCtrl.distribute_csr

  val pmp_check = VecInit(Seq.fill(total_tlb_ports)(
    Module(new PMPChecker(3, leaveHitMux = true)).io
  ))
  val tlbcsr_pmp = tlbcsr_dup.drop(2).map(RegNext(_))
  for (((p,d),i) <- (pmp_check zip dtlb_pmps).zipWithIndex) {
    p.apply(tlbcsr_pmp(i).priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    require(p.req.bits.size.getWidth == d.bits.size.getWidth)
  }
  val pmp_check_ptw = Module(new PMPCheckerv2(lgMaxSize = 3, sameCycle = false, leaveHitMux = true))
  pmp_check_ptw.io.apply(
    tlbcsr_pmp.last.priv.dmode,
    pmp.io.pmp, pmp.io.pma, io.ptw.resp.valid,
    Cat(io.ptw.resp.bits.data.entry.ppn, 0.U(12.W)).asUInt
  )
  dtlb.foreach(_.ptw_replenish := pmp_check_ptw.io.resp)

  val tdata = RegInit(VecInit(Seq.fill(TriggerNum)(0.U.asTypeOf(new MatchTriggerIO))))
//  val tEnable = RegInit(VecInit(Seq.fill(TriggerNum)(false.B)))
//  tEnable := csrCtrl.mem_trigger.tEnableVec
  val tEnable = RegNext(csrCtrl.mem_trigger.tEnableVec)
  when(csrCtrl.mem_trigger.tUpdate.valid) {
    tdata(csrCtrl.mem_trigger.tUpdate.bits.addr) := csrCtrl.mem_trigger.tUpdate.bits.tdata
  }

  val backendTriggerTimingVec = tdata.map(_.timing)
  val backendTriggerChainVec  = tdata.map(_.chain)

  //  val lTriggerMapping = Map(0 -> 2, 1 -> 3, 2 -> 5)
//  val sTriggerMapping = Map(0 -> 0, 1 -> 1, 2 -> 4)
//  val lChainMapping = Map(0 -> 2)
//  val sChainMapping = Map(0 -> 1)
  XSDebug(tEnable.asUInt.orR, "Debug Mode: At least one store trigger is enabled\n")
  for(j <- 0 until TriggerNum)
    PrintTriggerInfo(tEnable(j), tdata(j))
  // LoadUnit
  sludIssue.rsFeedback := DontCare
  for (i <- 0 until exuParameters.LduCnt) {
    loadUnits(i).io.bankConflictAvoidIn := (i % 2).U
    loadUnits(i).io.redirect := Pipe(redirectIn)
    lduIssues(i).rsFeedback.feedbackSlowLoad := loadUnits(i).io.feedbackSlow
    lduIssues(i).rsFeedback.feedbackFastLoad := loadUnits(i).io.feedbackFast
    val bnpi = outer.lduIssueNodes(i).in.head._2._1.bankNum / exuParameters.LduCnt
    val selSldu = sludIssue.issue.valid && sludIssue.rsIdx.bankIdxOH(bnpi * i + bnpi - 1, bnpi * i).orR
    loadUnits(i).io.rsIdx := Mux(selSldu, sludIssue.rsIdx, lduIssues(i).rsIdx)
    loadUnits(i).io.isFirstIssue := Mux(selSldu, sludIssue.rsFeedback.isFirstIssue, lduIssues(i).rsFeedback.isFirstIssue)
    // get input form dispatch
    loadUnits(i).io.ldin.valid := Mux(selSldu, sludIssue.issue.valid, lduIssues(i).issue.valid)
    loadUnits(i).io.ldin.bits := Mux(selSldu,sludIssue.issue.bits, lduIssues(i).issue.bits)
    loadUnits(i).io.auxValid := Mux(selSldu, sludIssue.auxValid, lduIssues(i).auxValid)
    sludIssue.issue.ready := loadUnits(i).io.ldin.ready
    lduIssues(i).issue.ready := loadUnits(i).io.ldin.ready
    when(selSldu){assert(lduIssues(i).issue.valid === false.B)}
    // dcache access
    loadUnits(i).io.dcache <> dcache.io.lsu.load(i)
    // forward
    loadUnits(i).io.lsq.forward <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer <> sbuffer.io.forward(i)
    // ld-ld violation check
    loadUnits(i).io.lsq.loadViolationQuery <> lsq.io.loadViolationQuery(i)
    loadUnits(i).io.csrCtrl       <> csrCtrl
    // dtlb
    loadUnits(i).io.tlb <> dtlb_reqs.take(exuParameters.LduCnt)(i)
    // pmp
    loadUnits(i).io.pmp <> pmp_check(i).resp
    //cancel
    io.earlyWakeUpCancel.foreach(w => w(i) := RegNext(loadUnits(i).io.cancel,false.B))
    // prefetch
    val pcDelay1Valid = RegNext(loadUnits(i).io.ldin.fire, false.B)
    val pcDelay1Bits = RegEnable(loadUnits(i).io.ldin.bits.uop.cf.pc, loadUnits(i).io.ldin.fire)
    val pcDelay2Bits = RegEnable(pcDelay1Bits, pcDelay1Valid)
    prefetcherOpt.foreach(pf => {
      pf.io.ld_in(i).valid := Mux(pf_train_on_hit,
        loadUnits(i).io.prefetch_train.valid,
        loadUnits(i).io.prefetch_train.valid && loadUnits(i).io.prefetch_train.bits.miss
      )
      pf.io.ld_in(i).bits := loadUnits(i).io.prefetch_train.bits
      pf.io.ld_in(i).bits.uop.cf.pc := Mux(loadUnits(i).io.s2IsPointerChasing,
        pcDelay1Bits,
        pcDelay2Bits)
    })

    // load to load fast forward: load(i) prefers data(i)
    val fastPriority = (i until exuParameters.LduCnt) ++ (0 until i)
    val fastValidVec = fastPriority.map(j => loadUnits(j).io.fastpathOut.valid)
    val fastDataVec = fastPriority.map(j => loadUnits(j).io.fastpathOut.data)
    val fastMatchVec = fastPriority.map(j => false.B)
    loadUnits(i).io.fastpathIn.valid := VecInit(fastValidVec).asUInt.orR
    loadUnits(i).io.fastpathIn.data := ParallelPriorityMux(fastValidVec, fastDataVec)
    val fastMatch = ParallelPriorityMux(fastValidVec, fastMatchVec)
    loadUnits(i).io.loadFastMatch := fastMatch
    loadUnits(i).io.loadFastImm := DontCare

    // Lsq to load unit's rs

    // passdown to lsq (load s1)
    lsq.io.loadPaddrIn(i) <> loadUnits(i).io.lsq.loadPaddrIn

    // passdown to lsq (load s2)
    lsq.io.loadIn(i) <> loadUnits(i).io.lsq.loadIn
    lsq.io.ldout(i) <> loadUnits(i).io.lsq.ldout
    lsq.io.ldRawDataOut(i) <> loadUnits(i).io.lsq.ldRawData
    lsq.io.s2_load_data_forwarded(i) <> loadUnits(i).io.lsq.s2_load_data_forwarded
    lsq.io.trigger(i) <> loadUnits(i).io.lsq.trigger

    // passdown to lsq (load s3)
    lsq.io.s2_dcache_require_replay(i) <> loadUnits(i).io.lsq.s2_dcache_require_replay
    lsq.io.s3_replay_from_fetch(i) <> loadUnits(i).io.lsq.s3_replay_from_fetch
    lsq.io.s3_delayed_load_error(i) <> loadUnits(i).io.s3_delayed_load_error

    // alter writeback exception info
    io.s3_delayed_load_error(i) := loadUnits(i).io.lsq.s3_delayed_load_error

    // update mem dependency predictor
    // io.memPredUpdate(i) := DontCare

    // --------------------------------
    // Load Triggers
    // --------------------------------
    val frontendTriggerTimingVec  = lduWritebacks(i).bits.uop.cf.trigger.frontendTiming
    val frontendTriggerChainVec   = lduWritebacks(i).bits.uop.cf.trigger.frontendChain
    val frontendTriggerHitVec     = lduWritebacks(i).bits.uop.cf.trigger.frontendHit
    val loadTriggerHitVec         = Wire(Vec(TriggerNum, Bool()))

    val triggerTimingVec  = VecInit(backendTriggerTimingVec.zip(frontendTriggerTimingVec).map { case (b, f) => b || f } )
    val triggerChainVec   = VecInit(backendTriggerChainVec.zip(frontendTriggerChainVec).map { case (b, f) => b || f } )
    val triggerHitVec     = VecInit(loadTriggerHitVec.zip(frontendTriggerHitVec).map { case (b, f) => b || f })

    val triggerCanFireVec = Wire(Vec(TriggerNum, Bool()))

    for (j <- 0 until TriggerNum) {
      loadUnits(i).io.trigger(j).tdata2     := tdata(j).tdata2
      loadUnits(i).io.trigger(j).matchType  := tdata(j).matchType
      loadUnits(i).io.trigger(j).tEnable    := tEnable(j) && tdata(j).load
      // Just let load triggers that match data unavailable
      loadTriggerHitVec(j) := loadUnits(i).io.trigger(j).addrHit && !tdata(j).select
    }
    TriggerCheckCanFire(TriggerNum, triggerCanFireVec, triggerHitVec, triggerTimingVec, triggerChainVec)

    lduWritebacks(i).bits.uop.cf.trigger.backendHit      := triggerHitVec
    lduWritebacks(i).bits.uop.cf.trigger.backendCanFire  := triggerCanFireVec
    XSDebug(lduWritebacks(i).bits.uop.cf.trigger.getBackendCanFire && lduWritebacks(i).valid, p"Debug Mode: Load Inst No.${i}" +
    p"has trigger fire vec ${lduWritebacks(i).bits.uop.cf.trigger.backendCanFire}\n")
  }
  // Prefetcher
  prefetcherOpt.foreach(pf => {
    dtlb_reqs(ld_tlb_ports - 1) <> pf.io.tlb_req
  })

  // StoreUnit
  for (i <- 0 until exuParameters.StuCnt) {
    val stu = storeUnits(i)

    stdUnits(i).io.in <> stdIssues(i).issue
    stdUnits(i).io.redirect := Pipe(redirectIn)

    stu.io.redirect     <> Pipe(redirectIn)
    staIssues(i).rsFeedback.feedbackSlowStore := stu.io.feedbackSlow
    stu.io.rsIdx        :=  staIssues(i).rsIdx
    // NOTE: just for dtlb's perf cnt
    stu.io.isFirstIssue := staIssues(i).rsFeedback.isFirstIssue
    stu.io.stin         <> staIssues(i).issue
    stu.io.lsq          <> lsq.io.storeIn(i)
    stu.io.lsq_replenish <> lsq.io.storeInRe(i)
    // dtlb
    stu.io.tlb          <> dtlb_reqs.drop(ld_tlb_ports)(i)
    stu.io.pmp          <> pmp_check(i+ld_tlb_ports).resp

    // Lsq to sta unit
    lsq.io.storeMaskIn(i) <> stu.io.storeMaskOut

    // Lsq to std unit's rs
    lsq.io.storeDataIn(i) := stData(i)


    // 1. sync issue info to store set LFST
    // 2. when store issue, broadcast issued sqPtr to wake up the following insts
    io.stIn(i).valid := staIssues(i).issue.valid
    io.stIn(i).bits := staIssues(i).issue.bits

    stu.io.stout.ready := true.B

    // -------------------------
    // Store Triggers
    // -------------------------
    val frontendTriggerTimingVec  = stOut(i).bits.uop.cf.trigger.frontendTiming
    val frontendTriggerChainVec   = stOut(i).bits.uop.cf.trigger.frontendChain
    val frontendTriggerHitVec     = stOut(i).bits.uop.cf.trigger.frontendHit

    val storeTriggerHitVec        = WireInit(VecInit(Seq.fill(TriggerNum)(false.B)))

    val triggerTimingVec  = VecInit(backendTriggerTimingVec.zip(frontendTriggerTimingVec).map { case (b, f) => b || f } )
    val triggerChainVec   = VecInit(backendTriggerChainVec.zip(frontendTriggerChainVec).map { case (b, f) => b || f } )
    val triggerHitVec     = VecInit(storeTriggerHitVec.zip(frontendTriggerHitVec).map { case (b, f) => b || f })

    val triggerCanFireVec = WireInit(VecInit(Seq.fill(TriggerNum)(false.B)))

    when(stOut(i).fire){
      for (j <- 0 until TriggerNum) {
        storeTriggerHitVec(j) := !tdata(j).select && TriggerCmp(
          stOut(i).bits.debug.vaddr,
          tdata(j).tdata2,
          tdata(j).matchType,
          tEnable(j) && tdata(j).store
        )
      }
      TriggerCheckCanFire(TriggerNum, triggerCanFireVec, triggerHitVec, triggerTimingVec, triggerChainVec)

      stOut(i).bits.uop.cf.trigger.backendHit := triggerHitVec
      stOut(i).bits.uop.cf.trigger.backendCanFire := triggerCanFireVec
    }
    // store data
  }

  // mmio store writeback will use store writeback port 0
  lsq.io.mmioStout.ready := false.B
  when (lsq.io.mmioStout.valid && !storeUnits(0).io.stout.valid) {
    stOut(0).valid := true.B
    stOut(0).bits  := lsq.io.mmioStout.bits
    lsq.io.mmioStout.ready := true.B
  }

  // Lsq
  lsq.io.rob            <> io.lsqio.rob
  lsq.io.enq            <> io.enqLsq
  lsq.io.brqRedirect    <> Pipe(redirectIn)
  staWritebacks.head.bits.redirectValid := lsq.io.rollback.valid
  staWritebacks.head.bits.redirect := lsq.io.rollback.bits
  staWritebacks.tail.foreach(e => {
    e.bits.redirectValid := false.B
    e.bits.redirect := DontCare
  })
  // lsq.io.uncache        <> uncache.io.lsq
  AddPipelineReg(lsq.io.uncache.req, uncache.io.lsq.req, false.B)
  AddPipelineReg(uncache.io.lsq.resp, lsq.io.uncache.resp, false.B)
  // delay dcache refill for 1 cycle for better timing
  // TODO: remove RegNext after fixing refill paddr timing
  // lsq.io.dcache         <> dcache.io.lsu.lsq
//  lsq.io.dcache         := RegNext(dcache.io.lsu.lsq)
  lsq.io.dcache.valid := RegNext(dcache.io.lsu.lsq.valid)
  lsq.io.dcache.bits := RegEnable(dcache.io.lsu.lsq.bits,dcache.io.lsu.lsq.valid)
  lsq.io.release        := dcache.io.lsu.release
  lsq.io.lqCancelCnt <> io.lqCancelCnt
  lsq.io.sqCancelCnt <> io.sqCancelCnt
  lsq.io.sqDeq <> io.sqDeq

  // LSQ to store buffer
  lsq.io.sbuffer        <> sbuffer.io.in
  lsq.io.sqempty        <> sbuffer.io.sqempty

  // Sbuffer
  sbuffer.io.csrCtrl    <> csrCtrl
  sbuffer.io.dcache     <> dcache.io.lsu.store
  // TODO: if dcache sbuffer resp needs to ne delayed
  // sbuffer.io.dcache.pipe_resp.valid := RegNext(dcache.io.lsu.store.pipe_resp.valid)
  // sbuffer.io.dcache.pipe_resp.bits := RegNext(dcache.io.lsu.store.pipe_resp.bits)

  // flush sbuffer
  val fenceFlush = io.fenceToSbuffer.flushSb
  val atomicsFlush = atomicsUnit.io.flush_sbuffer.valid
  io.fenceToSbuffer.sbIsEmpty := RegNext(sbuffer.io.flush.empty)
  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush))
  sbuffer.io.flush.valid := RegNext(fenceFlush || atomicsFlush)

  // AtomicsUnit: AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val s_normal :: s_atomics :: Nil = Enum(2)
  val state = RegInit(s_normal)

  atomicsUnit.io.in.valid := io.issueToMou.valid
  atomicsUnit.io.in.bits  := io.issueToMou.bits
  io.issueToMou.ready := atomicsUnit.io.in.ready
  atomicsUnit.io.redirect := Pipe(redirectIn)

  // TODO: complete amo's pmp support
  private val amoTlb = dtlb_ld(0).requestor(0)
  atomicsUnit.io.dtlb.resp.valid := false.B
  atomicsUnit.io.dtlb.resp.bits  := DontCare
  atomicsUnit.io.dtlb.req.ready  := amoTlb.req.ready
  atomicsUnit.io.pmpResp := pmp_check(0).resp

  atomicsUnit.io.dcache <> dcache.io.lsu.atomics
  atomicsUnit.io.flush_sbuffer.empty := sbuffer.io.flush.empty

  atomicsUnit.io.csrCtrl := csrCtrl

  // for atomicsUnit, it uses loadUnit(0)'s TLB port
  when(state === s_normal){
    when(atomicsUnit.io.in.valid){
      state := s_atomics
    }
  }

  when (state === s_atomics) {
    atomicsUnit.io.dtlb <> amoTlb
    when(atomicsUnit.io.out.fire){
      state := s_normal
    }
  }

  lsq.io.exceptionAddr.isStore := io.lsqio.exceptionAddr.isStore
  // Exception address is used several cycles after flush.
  // We delay it by 10 cycles to ensure its flush safety.
  val atomicsException = RegInit(false.B)
  when (DelayNWithCG(redirectIn.valid, 10) && atomicsException) {
    atomicsException := false.B
  }.elsewhen (atomicsUnit.io.exceptionAddr.valid) {
    atomicsException := true.B
  }
  val atomicsExceptionAddress = RegEnable(atomicsUnit.io.exceptionAddr.bits, atomicsUnit.io.exceptionAddr.valid)
  io.lsqio.exceptionAddr.vaddr := RegNext(Mux(atomicsException, atomicsExceptionAddress, lsq.io.exceptionAddr.vaddr))
  XSError(atomicsException && atomicsUnit.io.in.valid, "new instruction before exception triggers\n")

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  val mbistPipeline = if(coreParams.hasMbist && coreParams.hasShareBus) {
    Some(Module(new MBISTPipeline(4,s"${outer.parentName}_mbistPipe")))
  } else {
    None
  }

  io.lqFull := lsq.io.lqFull
  io.sqFull := lsq.io.sqFull

  val ldDeqCount = PopCount(lduIssues.map(_.issue.valid))
  val stDeqCount = PopCount(staIssues.map(_.issue.valid))
  val rsDeqCount = ldDeqCount + stDeqCount
  XSPerfAccumulate("load_rs_deq_count", ldDeqCount)
  XSPerfHistogram("load_rs_deq_count", ldDeqCount, true.B, 1, 2, 1)
  XSPerfAccumulate("store_rs_deq_count", stDeqCount)
  XSPerfHistogram("store_rs_deq_count", stDeqCount, true.B, 1, 2, 1)
  XSPerfAccumulate("ls_rs_deq_count", rsDeqCount)

  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.slice(16,24)

  val memBlockPerfEvents = Seq(
    ("ldDeqCount", ldDeqCount),
    ("stDeqCount", stDeqCount),
  )

  val perfFromUnits = (loadUnits ++ Seq(sbuffer, lsq, dcache)).flatMap(_.getPerfEvents)
  val perfFromIO    = io.perfEventsPTW.map(x => ("perfEventsPTW", x.value))
  val perfBlock     = Seq(("ldDeqCount", ldDeqCount),
                          ("stDeqCount", stDeqCount))
  // let index = 0 be no event
  val allPerfEvents = Seq(("noEvent", 0.U)) ++ perfFromUnits ++ perfFromIO ++ perfBlock

  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("MemBlock perfEvents Set", name, inc, i)
    }
  }

  val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  generatePerfEvent()
}
