package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import regfile.{PcMem, PcWritePort, RegFileTop}
import system.HasSoCParameter
import utils.{HPerfMonitor, HasPerfEvents, PerfEvent}
import xiangshan.backend.execute.exu.FenceIO
import xiangshan.{CommitType, ExuInput, HasXSParameter, L1CacheErrorInfo, MemPredUpdateReq, MicroOp, Redirect}
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

      val debug_int_rat = Input(Vec(32, UInt(PhyRegIdxWidth.W)))
      val debug_fp_rat = Input(Vec(32, UInt(PhyRegIdxWidth.W)))
    })
    private val intRs = integerReservationStation.module
    private val fpRs = floatingReservationStation.module
    private val memRs = memoryReservationStation.module
    private val intBlk = integerBlock.module
    private val fpBlk = floatingBlock.module
    private val memBlk = memoryBlock.module
    private val rf = regFile.module
    private val writeback = writebackNetwork.module

    rf.io.hartId := io.hartId
    rf.io.debug_int_rat := io.debug_int_rat
    rf.io.debug_fp_rat := io.debug_fp_rat
    private val localRedirect = writeback.io.redirectOut
    exuBlocks.foreach(_.module.redirectIn := Pipe(localRedirect))
    
    intRs.io.redirect := Pipe(localRedirect)
    intRs.io.loadEarlyWakeup := memRs.io.loadEarlyWakeup
    intRs.io.earlyWakeUpCancel := memBlk.io.earlyWakeUpCancel(0)
    intRs.io.integerAllocPregs := io.integerAllocPregs

    fpRs.io.redirect := Pipe(localRedirect)
    fpRs.io.loadEarlyWakeup := memRs.io.loadEarlyWakeup
    fpRs.io.earlyWakeUpCancel := memBlk.io.earlyWakeUpCancel(1)
    fpRs.io.floatingAllocPregs := io.floatingAllocPregs
    fpBlk.io.csr_frm := intBlk.io.csrio.fpu.frm

    memRs.io.redirect := Pipe(localRedirect)
    memRs.io.specWakeup.zip(intRs.io.specWakeup).foreach({case(a, b) => a := Pipe(b)})
    memRs.io.earlyWakeUpCancel := memBlk.io.earlyWakeUpCancel(2)
    memRs.io.integerAllocPregs := io.integerAllocPregs
    memRs.io.floatingAllocPregs := io.floatingAllocPregs
    memRs.io.stLastCompelet := memBlk.io.stIssuePtr

    intBlk.io.csrio.distributedUpdate(0) := memBlk.io.csrUpdate
    memBlk.io.csrCtrl <> intBlk.io.csrio.customCtrl
    memBlk.io.fenceToSbuffer <> intBlk.io.fenceio.sbuffer
    memBlk.io.sfence := intBlk.io.fenceio.sfence
    memBlk.io.tlbCsr <> intBlk.io.csrio.tlb

    memBlk.io.perfEventsPTW := io.perfEventsPTW
    io.memInfo := memBlk.io.memInfo
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

    private val exceptionReg = Pipe(io.csrio.exception)
    private val exceptionInUop = exceptionReg.bits.uop
    intBlk.io.fenceio <> io.fenceio
    intBlk.io.csrio <> io.csrio
    intBlk.io.csrio.exception := exceptionReg
    pcMem.io.read.last.addr := exceptionInUop.cf.ftqPtr.value
    intBlk.io.csrio.exception.bits.uop.cf.pc := pcMem.io.read.last.data.getPc(exceptionInUop.cf.ftqOffset)
    intBlk.io.csrio.memExceptionVAddr := memBlk.io.lsqio.exceptionAddr.vaddr
    memBlk.io.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(exceptionReg.bits.uop.ctrl.commitType)

    memBlk.io.issueToMou <> intBlk.io.issueToMou
    memBlk.io.writebackFromMou <> intBlk.io.writebackFromMou

    memBlk.redirectIn := Pipe(localRedirect)
    intBlk.redirectIn := Pipe(localRedirect)
    fpBlk.redirectIn := Pipe(localRedirect)

    io.redirectOut := writeback.io.redirectOut
    io.memPredUpdate := writeback.io.memPredUpdate

    private val pfevent = Module(new PFEvent)
    pfevent.io.distribute_csr := intBlk.io.csrio.customCtrl.distribute_csr.delay()
    private val csrevents = pfevent.io.hpmevent.slice(16,24)

    private val perfFromUnits = Seq(memBlk).flatMap(_.getPerfEvents)
    private val allPerfInc = perfFromUnits.map(_._2.asTypeOf(new PerfEvent))
    val perfEvents: Seq[(String, UInt)] = HPerfMonitor(csrevents, allPerfInc).getPerfEvents

    private val resetTree = ResetGenNode(
      Seq(
        ModuleNode(intRs),
        ModuleNode(fpRs),
        ModuleNode(memRs),
        ModuleNode(rf),
        ModuleNode(writeback),
        ModuleNode(intBlk),
        ModuleNode(fpBlk),
        ModuleNode(memBlk),
        ModuleNode(pcMem)
      )
    )
    ResetGen(resetTree, reset, Some(io.dfx_reset), !debugOpts.FPGAPlatform)
  }
}
