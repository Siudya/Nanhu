package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import regfile.{PcMem, PcWritePort, RegFileTop}
import system.HasSoCParameter
import utils.{HPerfMonitor, HasPerfEvents, PerfEvent}
import xiangshan.backend.execute.exu.FenceIO
import xiangshan.{ExuInput, HasXSParameter, L1CacheErrorInfo, MemPredUpdateReq, MicroOp, Redirect}
import xiangshan.backend.execute.exublock.{FloatingBlock, IntegerBlock, MemBlock}
import xiangshan.backend.execute.fu.csr.{CSRFileIO, PFEvent}
import xiangshan.backend.issue.FpRs.FloatingReservationStation
import xiangshan.backend.issue.IntRs.IntegerReservationStation
import xiangshan.backend.issue.MemRs.MemoryReservationStation
import xiangshan.backend.rob.RobLsqIO
import xiangshan.backend.writeback.WriteBackNetwork
import xiangshan.cache.mmu.BTlbPtwIO
import xiangshan.mem.{ExceptionAddrIO, LsqEnqIO}
import xs.utils.{DFTResetSignals, ModuleNode, ResetGen, ResetGenNode}
class ExecuteBlock(implicit p:Parameters) extends LazyModule with HasXSParameter{
  private val pcMemEntries = FtqSize
  val integerReservationStation: IntegerReservationStation = LazyModule(new IntegerReservationStation)
  val floatingReservationStation: FloatingReservationStation = LazyModule(new FloatingReservationStation)
  val memoryReservationStation: MemoryReservationStation = LazyModule(new MemoryReservationStation)
  private val integerBlock = LazyModule(new IntegerBlock)
  private val floatingBlock = LazyModule(new FloatingBlock)
  val memoryBlock: MemBlock = LazyModule(new MemBlock)
  private val regFile = LazyModule(new RegFileTop)
  val writebackNetwork: WriteBackNetwork = LazyModule(new WriteBackNetwork)
  private val exuBlocks = integerBlock :: floatingBlock :: memoryBlock :: Nil

  regFile.issueNode :*= integerReservationStation.issueNode
  regFile.issueNode :*= floatingReservationStation.issueNode
  regFile.issueNode :*= memoryReservationStation.issueNode
  for (eb <- exuBlocks) {
    eb.issueNode :*= regFile.issueNode
    writebackNetwork.node :=* eb.writebackNode
  }
  regFile.writebackNode :=* writebackNetwork.node
  floatingReservationStation.wakeupNode := writebackNetwork.node
  integerReservationStation.wakeupNode := writebackNetwork.node
  memoryReservationStation.wakeupNode := writebackNetwork.node
  lazy val module = new LazyModuleImp(this) with HasSoCParameter with HasPerfEvents{
    val io = IO(new Bundle {
      //Mem Block
      val l1Error = new L1CacheErrorInfo
      val lqCancelCnt = Output(UInt(log2Up(LoadQueueSize + 1).W))
      val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
      val sqDeq = Output(UInt(2.W))
      val stIn = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
      val enqLsq = new LsqEnqIO
      val ptw = new BTlbPtwIO(ld_tlb_ports + exuParameters.StuCnt)
      val lsqio = new Bundle {
        val exceptionAddr = new ExceptionAddrIO // to csr
        val rob = Flipped(new RobLsqIO) // rob to lsq
      }
      val memInfo = new Bundle {
        val sqFull = Output(Bool())
        val lqFull = Output(Bool())
        val dcacheMSHRFull = Output(Bool())
      }

      //Rename
      val integerAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
      val floatingAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))

      //Mdp update
      val memPredUpdate = Output(Valid(new MemPredUpdateReq))

      //Pc Mem Write
      val pcMemWrite = new PcWritePort

      val perfEventsPTW = Input(Vec(19, new PerfEvent))

      val redirectOut = Output(Valid(new Redirect))
      val fenceio = new FenceIO
      val csrio = new CSRFileIO
      val dfx_reset = Input(new DFTResetSignals())
    })
    private val localRedirect = writebackNetwork.module.io.redirectOut
    exuBlocks.foreach(_.module.redirectIn := Pipe(localRedirect))


    integerReservationStation.module.io.redirect := Pipe(localRedirect)
    integerReservationStation.module.io.loadEarlyWakeup := memoryReservationStation.module.io.loadEarlyWakeup
    integerReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel(0)
    integerReservationStation.module.io.integerAllocPregs := io.integerAllocPregs

    floatingReservationStation.module.io.redirect := Pipe(localRedirect)
    floatingReservationStation.module.io.loadEarlyWakeup := memoryReservationStation.module.io.loadEarlyWakeup
    floatingReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel(1)
    floatingReservationStation.module.io.floatingAllocPregs := io.floatingAllocPregs
    floatingBlock.module.io.csr_frm := integerBlock.module.io.csrio.fpu.frm

    memoryReservationStation.module.io.redirect := Pipe(localRedirect)
    memoryReservationStation.module.io.specWakeup.zip(integerReservationStation.module.io.specWakeup).foreach({case(a, b) => a := Pipe(b)})
    memoryReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel(2)
    memoryReservationStation.module.io.integerAllocPregs := io.integerAllocPregs
    memoryReservationStation.module.io.floatingAllocPregs := io.floatingAllocPregs


    integerBlock.module.io.csrio.distributedUpdate(0) := memoryBlock.module.io.csrUpdate
    memoryBlock.module.io.csrCtrl <> integerBlock.module.io.csrio.customCtrl
    memoryBlock.module.io.fenceToSbuffer <> integerBlock.module.io.fenceio.sbuffer
    memoryBlock.module.io.sfence := integerBlock.module.io.fenceio.sfence
    memoryBlock.module.io.tlbCsr <> integerBlock.module.io.csrio.tlb

    memoryBlock.module.io.perfEventsPTW := io.perfEventsPTW
    io.memInfo := memoryBlock.module.io.memInfo
    io.ptw <> memoryBlock.module.io.ptw
    io.l1Error := memoryBlock.module.io.error
    io.lqCancelCnt := memoryBlock.module.io.lqCancelCnt
    io.sqCancelCnt := memoryBlock.module.io.sqCancelCnt
    io.sqDeq := memoryBlock.module.io.sqDeq
    io.stIn := memoryBlock.module.io.stIn
    io.enqLsq <> memoryBlock.module.io.enqLsq
    io.lsqio <> memoryBlock.module.io.lsqio

    //issue + redirect + exception
    private val pcReadPortNum = regFile.module.pcReadNum + writebackNetwork.module.io.pcReadData.length + 1
    private val pcMem = Module(new PcMem(pcReadPortNum, 1))
    pcMem.io.write.head := io.pcMemWrite

    pcMem.io.read.take(pcReadPortNum - 1).zip(regFile.module.io.pcReadAddr ++ writebackNetwork.module.io.pcReadAddr).foreach({ case (r, addr) => r.addr := addr })
    (regFile.module.io.pcReadData ++ writebackNetwork.module.io.pcReadData).zip(pcMem.io.read.take(pcReadPortNum - 1)).foreach({ case (data, r) => data := r.data })

    private val exceptionInUop = io.csrio.exception.bits.uop
    integerBlock.module.io.fenceio <> io.fenceio
    integerBlock.module.io.csrio <> io.csrio
    pcMem.io.read.last.addr := exceptionInUop.cf.ftqPtr.value
    integerBlock.module.io.csrio.exception.bits.uop.cf.pc := pcMem.io.read.last.data.getPc(exceptionInUop.cf.ftqOffset)

    memoryBlock.module.io.issueToMou <> integerBlock.module.io.issueToMou
    memoryBlock.module.io.writebackFromMou <> integerBlock.module.io.writebackFromMou

    memoryBlock.module.redirectIn := Pipe(localRedirect)
    integerBlock.module.redirectIn := Pipe(localRedirect)
    floatingBlock.module.redirectIn := Pipe(localRedirect)

    io.redirectOut := writebackNetwork.module.io.redirectOut
    io.memPredUpdate := writebackNetwork.module.io.memPredUpdate

    private val pfevent = Module(new PFEvent)
    pfevent.io.distribute_csr := integerBlock.module.io.csrio.customCtrl.distribute_csr.delay()
    private val csrevents = pfevent.io.hpmevent.slice(16,24)

    private val perfFromUnits = Seq(memoryBlock.module).flatMap(_.getPerfEvents)
    private val allPerfInc = perfFromUnits.map(_._2.asTypeOf(new PerfEvent))
    val perfEvents: Seq[(String, UInt)] = HPerfMonitor(csrevents, allPerfInc).getPerfEvents

    private val resetTree = ResetGenNode(
      Seq(
        ModuleNode(integerReservationStation.module),
        ModuleNode(floatingReservationStation.module),
        ModuleNode(memoryReservationStation.module),
        ModuleNode(regFile.module),
        ModuleNode(writebackNetwork.module),
        ModuleNode(integerBlock.module),
        ModuleNode(floatingBlock.module),
        ModuleNode(memoryBlock.module),
        ModuleNode(pcMem)
      )
    )
    ResetGen(resetTree, reset, Some(io.dfx_reset), !debugOpts.FPGAPlatform)
  }
}
