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

package xiangshan

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.tilelink.TLBuffer
import xs.utils.mbist.MBISTPipeline
import xs.utils.sram.SRAMTemplate
import xs.utils.{ModuleNode, ResetGen, ResetGenNode, DFTResetSignals}
import system.HasSoCParameter
import utils._
import xiangshan.backend._
import xiangshan.cache.mmu._
import xiangshan.frontend._

abstract class XSModule(implicit val p: Parameters) extends MultiIOModule
  with HasXSParameter
  with HasFPUParameters {
  def io: Record
}

//remove this trait after impl module logic
trait NeedImpl {
  this: RawModule =>
  override protected def IO[T <: Data](iodef: T): T = {
    println(s"[Warn]: (${this.name}) please reomve 'NeedImpl' after implement this module")
    val io = chisel3.experimental.IO(iodef)
    io <> DontCare
    io
  }
}

abstract class XSBundle(implicit val p: Parameters) extends Bundle
  with HasXSParameter

abstract class XSCoreBase(val parentName:String = "Unknown")(implicit p: config.Parameters) extends LazyModule
  with HasXSParameter
{
  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val debug_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))
  val plic_int_sink = IntSinkNode(IntSinkPortSimple(2, 1))
  // outer facing nodes
  val frontend = LazyModule(new Frontend(parentName = parentName + "frontend_"))
  val ptw = LazyModule(new PTWWrapper(parentName = parentName + "ptw_"))
  val ptw_to_l2_buffer = LazyModule(new TLBuffer)
  val csrOut = BundleBridgeSource(Some(() => new DistributedCSRIO()))
  val exuBlock = LazyModule(new ExecuteBlock(parentName = parentName + "execute_"))
  val ctrlBlock = LazyModule(new CtrlBlock)
  exuBlock.integerReservationStation.dispatchNode :*= ctrlBlock.dispatchNode
  exuBlock.floatingReservationStation.dispatchNode :*= ctrlBlock.dispatchNode
  exuBlock.memoryReservationStation.dispatchNode :*= ctrlBlock.dispatchNode
  ctrlBlock.rob.writebackNode :=* exuBlock.writebackNetwork.node
  ptw_to_l2_buffer.node := ptw.node
}

class XSCore(parentName:String = "Unknown")(implicit p: config.Parameters) extends XSCoreBase(parentName = parentName)
  with HasXSDts
{
  lazy val module = new XSCoreImp(this)
}

class XSCoreImp(outer: XSCoreBase) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasSoCParameter {
  val io = IO(new Bundle {
    val hartId = Input(UInt(64.W))
    val cpu_halt = Output(Bool())
    val l2_pf_enable = Output(Bool())
    val perfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
    val beu_errors = Output(new XSL1BusErrors())
    val dfx_reset = Input(new DFTResetSignals())
  })

  println(s"FPGAPlatform:${env.FPGAPlatform} EnableDebug:${env.EnableDebug}")

  private val frontend = outer.frontend.module
  private val ctrlBlock = outer.ctrlBlock.module
  private val exuBlock = outer.exuBlock.module
  private val ptw = outer.ptw.module
  private val ptw_to_l2_buffer = outer.ptw_to_l2_buffer.module
  private val csrioIn = exuBlock.io.csrio
  private val fenceio = exuBlock.io.fenceio

  frontend.io.hartId  := io.hartId
  ctrlBlock.io.hartId := io.hartId
  exuBlock.io.csrio.hartId := io.hartId
  io.cpu_halt := ctrlBlock.io.cpu_halt
  exuBlock.io.dfx_reset := io.dfx_reset
  exuBlock.io.hartId := io.hartId

  io.beu_errors.icache := frontend.io.error.toL1BusErrorUnitInfo()
  io.beu_errors.dcache := exuBlock.io.l1Error.toL1BusErrorUnitInfo()

  frontend.io.backend <> ctrlBlock.io.frontend
  frontend.io.sfence := fenceio.sfence
  frontend.io.tlbCsr <> csrioIn.tlb
  frontend.io.csrCtrl <> csrioIn.customCtrl
  frontend.io.fencei := fenceio.fencei

  ctrlBlock.io.csrCtrl <> csrioIn.customCtrl
  ctrlBlock.io.lqCancelCnt := exuBlock.io.lqCancelCnt
  ctrlBlock.io.sqCancelCnt := exuBlock.io.sqCancelCnt
  ctrlBlock.io.sqDeq := exuBlock.io.sqDeq
  ctrlBlock.io.stIn := exuBlock.io.stIn
  exuBlock.io.enqLsq <> ctrlBlock.io.enqLsq
  ctrlBlock.io.redirectIn := exuBlock.io.redirectOut
  exuBlock.io.floatingAllocPregs.zip(exuBlock.io.integerAllocPregs).zip(ctrlBlock.io.allocPregs).foreach({case((f, i), r) =>
    f.valid := r.isFp
    f.bits := r.preg
    i.valid := r.isInt
    i.bits := r.preg
  })
  ctrlBlock.io.memPredUpdate := exuBlock.io.memPredUpdate
  exuBlock.io.debug_int_rat := ctrlBlock.io.debug_int_rat
  exuBlock.io.debug_fp_rat := ctrlBlock.io.debug_fp_rat

  exuBlock.io.perfEventsPTW  := ptw.getPerf

  csrioIn.hartId := io.hartId
  csrioIn.perf := DontCare
  csrioIn.perf.retiredInstr := ctrlBlock.io.robio.toCSR.perfinfo.retiredInstr
  csrioIn.perf.ctrlInfo := ctrlBlock.io.perfInfo.ctrlInfo
  csrioIn.perf.memInfo := exuBlock.io.memInfo
  csrioIn.perf.frontendInfo := frontend.io.frontendInfo

  csrioIn.perf.perfEventsFrontend := frontend.getPerf
  csrioIn.perf.perfEventsCtrl     := ctrlBlock.getPerf
  csrioIn.perf.perfEventsLsu      := exuBlock.getPerf
  csrioIn.perf.perfEventsHc       := io.perfEvents

  csrioIn.fpu.fflags := ctrlBlock.io.robio.toCSR.fflags
  csrioIn.fpu.isIllegal := false.B
  csrioIn.fpu.dirty_fs := ctrlBlock.io.robio.toCSR.dirty_fs
  csrioIn.exception := ctrlBlock.io.robio.exception
  ctrlBlock.io.robio.toCSR.intrBitSet := csrioIn.interrupt
  ctrlBlock.io.robio.toCSR.wfiEvent := csrioIn.wfi_event

  csrioIn.externalInterrupt.msip := outer.clint_int_sink.in.head._1(0)
  csrioIn.externalInterrupt.mtip := outer.clint_int_sink.in.head._1(1)
  csrioIn.externalInterrupt.meip := outer.plic_int_sink.in.head._1(0)
  csrioIn.externalInterrupt.seip := outer.plic_int_sink.in.last._1(0)
  csrioIn.externalInterrupt.debug := outer.debug_int_sink.in.head._1(0)

  csrioIn.distributedUpdate(1).w.valid := frontend.io.csrUpdate.w.valid
  csrioIn.distributedUpdate(1).w.bits := frontend.io.csrUpdate.w.bits

  exuBlock.io.rob := ctrlBlock.io.robio.lsq
  exuBlock.io.pcMemWrite.en := RegNext(frontend.io.backend.fromFtq.pc_mem_wen, false.B)
  exuBlock.io.pcMemWrite.addr := RegEnable(frontend.io.backend.fromFtq.pc_mem_waddr, frontend.io.backend.fromFtq.pc_mem_wen)
  exuBlock.io.pcMemWrite.data := RegEnable(frontend.io.backend.fromFtq.pc_mem_wdata, frontend.io.backend.fromFtq.pc_mem_wen)

  private val itlbRepeater1 = PTWRepeater(frontend.io.ptw, fenceio.sfence, csrioIn.tlb)
  private val itlbRepeater2 = PTWRepeater(itlbRepeater1.io.ptw, ptw.io.tlb(0), fenceio.sfence, csrioIn.tlb)
  private val dtlbRepeater1  = PTWFilter(exuBlock.io.ptw, fenceio.sfence, csrioIn.tlb, l2tlbParams.filterSize)
  private val dtlbRepeater2  = PTWRepeaterNB(passReady = false, dtlbRepeater1.io.ptw, ptw.io.tlb(1), fenceio.sfence, csrioIn.tlb)
  ptw.io.sfence := fenceio.sfence
  ptw.io.csr.tlb <> csrioIn.tlb
  ptw.io.csr.distribute_csr <> csrioIn.customCtrl.distribute_csr
  ptw.io.csr.prefercache <> csrioIn.customCtrl.ptw_prefercache_enable

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
  io.l2_pf_enable := csrioIn.customCtrl.l2_pf_enable

  val mbistPipeline = if(coreParams.hasMbist && coreParams.hasShareBus) {
    Some(Module(new MBISTPipeline(Int.MaxValue,s"MBIST_Core")))
  } else {
    None
  }
  val mbist = if(coreParams.hasMbist && coreParams.hasShareBus) Some(IO(mbistPipeline.get.io.mbist.get.cloneType)) else None
  if(coreParams.hasMbist && coreParams.hasShareBus){
    mbist.get <> mbistPipeline.get.io.mbist.get
  }
  val sigFromSrams = if(coreParams.hasMbist) Some(SRAMTemplate.genBroadCastBundleTop()) else None
  val dft = if(coreParams.hasMbist) Some(IO(sigFromSrams.get.cloneType)) else None
  if(coreParams.hasMbist) {
    dft.get <> sigFromSrams.get
    dontTouch(dft.get)
  }
  // Modules are reset one by one
  private val resetTree = ResetGenNode(
    Seq(
      ModuleNode(exuBlock), ModuleNode(dtlbRepeater1),
      ResetGenNode(Seq(
        ModuleNode(itlbRepeater2),
        ModuleNode(ptw),
        ModuleNode(dtlbRepeater2),
        ModuleNode(ptw_to_l2_buffer),
      )),
      ResetGenNode(Seq(
        ResetGenNode(Seq(
          ModuleNode(ctrlBlock),
          ResetGenNode(Seq(
            ModuleNode(frontend), ModuleNode(itlbRepeater1)
          ))
        ))
      ))
    )
  )

  ResetGen(resetTree, reset, Some(io.dfx_reset), !debugOpts.FPGAPlatform)

}
