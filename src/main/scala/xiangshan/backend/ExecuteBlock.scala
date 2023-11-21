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
package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import regfile.{PcMem, PcWritePort, RegFileTop}
import system.HasSoCParameter
import utils.{HPerfMonitor, HasPerfEvents, PerfEvent}
import xiangshan.backend.execute.exu.FenceIO
import xiangshan.{CommitType, DistributedCSRUpdateReq, ExuInput, ExuOutput, HasXSParameter, L1CacheErrorInfo, MemPredUpdateReq, MicroOp, Redirect, XSCoreParamsKey}
import xiangshan.backend.execute.exublock.{FloatingBlock, IntegerBlock, MemBlock}
import xiangshan.backend.execute.fu.csr.CSRConst.ModeM
import xiangshan.backend.execute.fu.csr.CSRFileIO
import xiangshan.backend.issue.FpRs.FloatingReservationStation
import xiangshan.backend.issue.IntRs.IntegerReservationStation
import xiangshan.backend.issue.MemRs.MemoryReservationStation
import xiangshan.backend.rob.RobLsqIO
import xiangshan.backend.writeback.WriteBackNetwork
import xiangshan.cache.mmu.BTlbPtwIO
import xiangshan.mem.LsqEnqIO
import xiangshan.vector.HasVectorParameters
import xiangshan.vector.vbackend.vexecute.{VectorBlock, VectorPermutationBlock}
import xiangshan.vector.vbackend.vissue.vrs.VectorReservationStation
import xiangshan.vector.vbackend.vregfile.VRegfileTop
import xs.utils.{DFTResetSignals, ModuleNode, ResetGen, ResetGenNode}
import xiangshan.mem._
class ExecuteBlock(val parentName:String = "Unknown")(implicit p:Parameters) extends LazyModule with HasXSParameter with HasVectorParameters {
  val integerReservationStation: IntegerReservationStation = LazyModule(new IntegerReservationStation)
  val floatingReservationStation: FloatingReservationStation = LazyModule(new FloatingReservationStation)
  val memoryReservationStation: MemoryReservationStation = LazyModule(new MemoryReservationStation)
  val vectorReservationStation: VectorReservationStation = LazyModule(new VectorReservationStation)

  val integerBlock: IntegerBlock = LazyModule(new IntegerBlock)
  val floatingBlock: FloatingBlock = LazyModule(new FloatingBlock)
  val vectorBlock: VectorBlock = LazyModule(new VectorBlock)
  val vectorPermutationBlock: VectorPermutationBlock = LazyModule(new VectorPermutationBlock)
  val memoryBlock: MemBlock = LazyModule(new MemBlock(parentName + "memBlock_"))
  val exuBlocks = integerBlock :: floatingBlock :: memoryBlock :: Nil

  val regFile = LazyModule(new RegFileTop(2))
  val vRegFile = LazyModule(new VRegfileTop(loadUnitNum * 2 + 1))

  val writebackNetwork: WriteBackNetwork = LazyModule(new WriteBackNetwork)

  regFile.issueNode :*= integerReservationStation.issueNode
  regFile.issueNode :*= floatingReservationStation.issueNode
  regFile.issueNode :*= memoryReservationStation.issueNode
  vRegFile.issueNode :*= vectorReservationStation.issueNode

  for (eb <- exuBlocks) {
    eb.issueNode :*= regFile.issueNode
    writebackNetwork.node :=* eb.writebackNode
  }

  vectorBlock.issueNode :*= vRegFile.issueNode

  memoryBlock.vlduWritebackNodes.foreach(vldwb => vRegFile.writebackMergeNode :=* vldwb)
  memoryBlock.vstaWritebackNodes.foreach(vstawb => vRegFile.writebackMergeNode :=* vstawb)
  memoryBlock.vstdWritebackNodes.foreach(vstdwb => vRegFile.writebackMergeNode :=* vstdwb)
  vRegFile.writebackMergeNode :=* vectorPermutationBlock.writebackNode
  vRegFile.writebackMergeNode :=* vectorBlock.writebackNode

  writebackNetwork.node :=* vRegFile.writebackMergeNode

  regFile.writebackNode :=* writebackNetwork.node

  floatingReservationStation.wakeupNode := writebackNetwork.node
  integerReservationStation.wakeupNode := writebackNetwork.node
  memoryReservationStation.wakeupNode := writebackNetwork.node
  vectorPermutationBlock.vprs.wakeupNode := writebackNetwork.node
  vectorReservationStation.wakeupNode := writebackNetwork.node
  lazy val module = new ExecuteBlockImp(this)
}

class ExecuteBlockImp(outer:ExecuteBlock) extends LazyModuleImp(outer)
  with HasSoCParameter with HasVectorParameters{
  val io = IO(new Bundle {
    val hartId = Input(UInt(64.W))
    //Mem Block
    val l1Error = new L1CacheErrorInfo
    val lqCancelCnt = Output(UInt(log2Up(LoadQueueSize + 1).W))
    val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
    val sqDeq = Output(UInt(2.W))
    val stIn = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
    val enqLsq = new LsqEnqIO
    val ptw = new BTlbPtwIO(ld_tlb_ports + exuParameters.StuCnt)
    val rob = Flipped(new RobLsqIO) // rob to lsq
    val lsqVecDeqCnt = Output(new LsqVecDeqIO)
    val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))

    //Rename
    val integerAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val floatingAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val vectorAllocPregs = Vec(coreParams.vectorParameters.vRenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))

    //Mdp update
    val memPredUpdate = Output(Valid(new MemPredUpdateReq))

    //Pc Mem Write
    val pcMemWrite = new PcWritePort

    val perfEventsPTW = Input(Vec(19, new PerfEvent))

    val vecFaultOnlyFirst = Flipped(ValidIO(new ExuOutput))
    val redirectOut = Output(Valid(new Redirect))
    val preWalk = Output(Valid(new Redirect))
    val fenceio = new FenceIO
    val csrio = new CSRFileIO
    val memBlk_csrUpdate = Output(new DistributedCSRUpdateReq)  //connect to ExuBlock.io.csrio
    val prefetchI = Output(Valid(UInt(p(XSCoreParamsKey).XLEN.W)))
    val dfx_reset = Input(new DFTResetSignals())

    val debug_int_rat = Input(Vec(32, UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Input(Vec(32, UInt(PhyRegIdxWidth.W)))
    val debug_vec_rat = Input(Vec(32, UInt(PhyRegIdxWidth.W)))
  })
  private val intRs = outer.integerReservationStation.module
  private val fpRs = outer.floatingReservationStation.module
  private val memRs = outer.memoryReservationStation.module
  private val vRs = outer.vectorReservationStation.module
  private val vpRs = outer.vectorPermutationBlock.vprs.module

  private val intBlk = outer.integerBlock.module
  private val fpBlk = outer.floatingBlock.module
  private val memBlk = outer.memoryBlock.module
  private val vecBlk = outer.vectorBlock.module
  private val vpBlk = outer.vectorPermutationBlock.module

  private val rf = outer.regFile.module
  private val vrf = outer.vRegFile.module

  private val writeback = outer.writebackNetwork.module
  writeback.io.vecFaultOnlyFirst := io.vecFaultOnlyFirst
  io.preWalk.valid := writeback.io.preWalk.valid & !RegNext(writeback.io.redirectOut.valid, false.B)
  io.preWalk.bits := writeback.io.preWalk.bits

  private val localRedirect = writeback.io.redirectOut
  rf.io.hartId := io.hartId
  rf.io.debug_int_rat := io.debug_int_rat
  rf.io.debug_fp_rat := io.debug_fp_rat
  rf.io.extraReads.take(vrf.rfReadNum).zip(vrf.io.scalarReads).foreach({ case (a, b) => a <> b })
  rf.io.extraReads.last <> vpBlk.io.rfReadPort.srf
  rf.io.redirect := Pipe(localRedirect)
  rf.io.mmuEnable := intBlk.io.csrio.tlb.satp.mode =/= 0.U && intBlk.io.csrio.tlb.priv.dmode < ModeM

  vrf.io.hartId := io.hartId
  vrf.io.debug_vec_rat := io.debug_vec_rat
  vrf.io.vectorReads.take(loadUnitNum * 2).zip(rf.io.vectorReads).foreach({ case (a, b) => a <> b })
  vrf.io.vectorReads.last <> vpBlk.io.rfReadPort.vrf
  for (elem <- vrf.io.moveOldValReqs.zip(rf.io.vectorRfMoveReq)) {
    elem._1 := Pipe(elem._2)
  }
  vrf.io.redirect := Pipe(localRedirect)
  vrf.io.vecAllocPregs := io.vectorAllocPregs

  outer.exuBlocks.foreach(_.module.redirectIn := Pipe(localRedirect))

  intRs.io.redirect := Pipe(localRedirect)
  intRs.io.loadEarlyWakeup := memRs.io.loadEarlyWakeup
  intRs.io.earlyWakeUpCancel := memBlk.io.earlyWakeUpCancel(0)
  intRs.io.integerAllocPregs := io.integerAllocPregs

  fpRs.io.redirect := Pipe(localRedirect)
  fpRs.io.loadEarlyWakeup := memRs.io.loadEarlyWakeup
  fpRs.io.earlyWakeUpCancel := memBlk.io.earlyWakeUpCancel(1)
  fpRs.io.floatingAllocPregs := io.floatingAllocPregs
  fpRs.io.mulSpecWakeUp.zip(intRs.io.mulSpecWakeup).foreach({ case (a, b) => a := Pipe(b) })
  fpBlk.io.csr_frm := intBlk.io.csrio.fpu.frm

  memRs.io.redirect := Pipe(localRedirect)
  memRs.io.aluJmpSpecWakeup := intRs.io.aluJmpSpecWakeup
  memRs.io.mulSpecWakeup.zip(intRs.io.mulSpecWakeup).foreach({ case (a, b) => a := Pipe(b) })
  memRs.io.fmaSpecWakeup := fpRs.io.fmacSpecWakeUp
  memRs.io.earlyWakeUpCancel := memBlk.io.earlyWakeUpCancel(2)
  memRs.io.integerAllocPregs := io.integerAllocPregs
  memRs.io.floatingAllocPregs := io.floatingAllocPregs
  memRs.io.vectorAllocPregs := io.vectorAllocPregs
  memRs.io.stLastCompelet := memBlk.io.stIssuePtr

  vRs.io.redirect := Pipe(localRedirect)
  vRs.io.intAllocPregs := io.integerAllocPregs
  vRs.io.fpAllocPregs := io.floatingAllocPregs
  vRs.io.vecAllocPregs := io.vectorAllocPregs

  vpBlk.io.redirect := Pipe(localRedirect)
  vpBlk.io.intAllocPregs := io.integerAllocPregs
  vpBlk.io.fpAllocPregs := io.floatingAllocPregs
  vpBlk.io.vecAllocPregs := io.vectorAllocPregs
  vpBlk.io.frm := intBlk.io.csrio.fpu.frm
  vpBlk.io.vstart := intBlk.io.csrio.vcsr.vstart
  vpBlk.io.vcsr := io.csrio.vcsr.vcsr

  vecBlk.io.frm := intBlk.io.csrio.fpu.frm
  vecBlk.io.vstart := intBlk.io.csrio.vcsr.vstart
  vecBlk.io.vcsr := io.csrio.vcsr.vcsr

  io.memBlk_csrUpdate := memBlk.io.csrUpdate
  memBlk.io.csrCtrl <> intBlk.io.csrio.customCtrl
  memBlk.io.fenceToSbuffer <> intBlk.io.fenceio.sbuffer
  memBlk.io.sfence := intBlk.io.fenceio.sfence
  memBlk.io.tlbCsr <> intBlk.io.csrio.tlb
  memBlk.io.hartId := io.hartId
  io.lqDeq := RegNext(memBlk.io.lqDeq)

  memBlk.io.sqWbout(0).ready := true.B
  memBlk.io.sqWbout(1).ready := true.B

  io.lsqVecDeqCnt <> memBlk.io.lsqVecDeqCnt

  memBlk.io.perfEventsPTW := io.perfEventsPTW
  io.ptw <> memBlk.io.ptw
  io.l1Error := memBlk.io.error
  io.lqCancelCnt := memBlk.io.lqCancelCnt
  io.sqCancelCnt := memBlk.io.sqCancelCnt
  io.sqDeq := memBlk.io.sqDeq
  io.stIn := memBlk.io.stIn
  io.enqLsq <> memBlk.io.enqLsq
  io.rob <> memBlk.io.lsqio.rob

  //issue + redirect + exception
  private val pcReadPortNum = rf.pcReadNum + writeback.io.pcReadData.length + 1
  private val pcMem = Module(new PcMem(pcReadPortNum, 1))
  pcMem.io.write.head := io.pcMemWrite

  pcMem.io.read.take(pcReadPortNum - 1).zip(rf.io.pcReadAddr ++ writeback.io.pcReadAddr).foreach({ case (r, addr) => r.addr := addr })
  (rf.io.pcReadData ++ writeback.io.pcReadData).zip(pcMem.io.read.take(pcReadPortNum - 1)).foreach({ case (data, r) => data := r.data })

  io.prefetchI := intBlk.io.prefetchI
  private val exceptionReg = Pipe(io.csrio.exception)
  private val exceptionInUop = exceptionReg.bits.uop
  intBlk.io.fenceio <> io.fenceio
  intBlk.io.fenceio.sbuffer.sbIsEmpty := memBlk.io.fenceToSbuffer.sbIsEmpty
  intBlk.io.csrio <> io.csrio
  intBlk.io.csrio.exception := exceptionReg
  pcMem.io.read.last.addr := exceptionInUop.cf.ftqPtr.value
  intBlk.io.csrio.exception.bits.uop.cf.pc := pcMem.io.read.last.data.getPc(exceptionInUop.cf.ftqOffset)
  intBlk.io.csrio.memExceptionVAddr := memBlk.io.lsqio.exceptionAddr.vaddr
  memBlk.io.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(exceptionReg.bits.uop.ctrl.commitType)

  memBlk.io.issueToMou <> intBlk.io.issueToMou
  memBlk.io.writebackFromMou <> intBlk.io.writebackFromMou

  memBlk.redirectIn := localRedirect
  intBlk.redirectIn := localRedirect
  fpBlk.redirectIn := localRedirect
  vecBlk.redirectIn := localRedirect

  io.redirectOut := writeback.io.redirectOut
  io.memPredUpdate := writeback.io.memPredUpdate

  intBlk.io.csrio.perf.perfEventsLsu := memBlk.getPerf
  intBlk.io.csrio.perf.memInfo := memBlk.io.memInfo

  private val resetTree = ResetGenNode(
    Seq(
      ResetGenNode(Seq(
        ModuleNode(intRs),
        ModuleNode(fpRs),
        ModuleNode(memRs),
        ModuleNode(vRs),
        ModuleNode(vpBlk)
      )),
      ResetGenNode(Seq(
        ModuleNode(intBlk)
      )),
      ResetGenNode(Seq(
        ModuleNode(fpBlk)
      )),
      ResetGenNode(Seq(
        ModuleNode(memBlk)
      )),
      ResetGenNode(Seq(
        ModuleNode(vrf),
        ModuleNode(vecBlk)
      )),
      ResetGenNode(Seq(
        ModuleNode(rf),
        ModuleNode(pcMem),
        ModuleNode(writeback)
      ))
    )
  )
  ResetGen(resetTree, reset, Some(io.dfx_reset), !debugOpts.FPGAPlatform)
}