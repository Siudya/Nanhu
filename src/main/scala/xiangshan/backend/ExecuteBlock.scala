package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import regfile.{PcMem, PcWritePort, RegFileTop}
import xiangshan.backend.execute.exu.FenceIO
import xiangshan.{MicroOp, Redirect}
import xiangshan.backend.execute.exublock.{FloatingBlock, IntegerBlock, MemBlock}
import xiangshan.backend.execute.fu.csr.CSRFileIO
import xiangshan.backend.issue.FpRs.FloatingReservationStation
import xiangshan.backend.issue.IntRs.IntegerReservationStation
import xiangshan.backend.issue.MemRs.MemoryReservationStation
import xiangshan.backend.writeback.WriteBackNetwork
import xiangshan.backend.issue.{FpRs, IntRs, MemRs}
class ExecuteBlock(implicit p:Parameters) extends LazyModule {
  private val pcMemEntries = 64
  private val integerReservationStation = LazyModule(new IntegerReservationStation)
  private val floatingReservationStation = LazyModule(new FloatingReservationStation)
  private val memoryReservationStation = LazyModule(new MemoryReservationStation)
  private val integerBlock = LazyModule(new IntegerBlock)
  private val floatingBlock = LazyModule(new FloatingBlock)
  private val memoryBlock = LazyModule(new MemBlock)
  private val regFile = LazyModule(new RegFileTop)
  private val writebackNetwork = LazyModule(new WriteBackNetwork)
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
  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle {
      val redirectOut = Output(Valid(new Redirect))
      val fenceio = new FenceIO
      val csrio = new CSRFileIO
      val pcMemWrite = new PcWritePort
    })
    private val localRedirect = writebackNetwork.module.io.redirectOut
    exuBlocks.foreach(_.module.redirectIn := Pipe(localRedirect))

    integerReservationStation.module.io.redirect := Pipe(localRedirect)
    integerReservationStation.module.io.loadEarlyWakeup := memoryReservationStation.module.io.loadEarlyWakeup
    integerReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel(0)

    floatingReservationStation.module.io.redirect := Pipe(localRedirect)
    floatingReservationStation.module.io.loadEarlyWakeup := memoryReservationStation.module.io.loadEarlyWakeup
    floatingReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel(1)

    memoryReservationStation.module.io.redirect := Pipe(localRedirect)
    memoryReservationStation.module.io.specWakeup.zip(integerReservationStation.module.io.specWakeup).foreach({case(a, b) => a := Pipe(b)})
    memoryReservationStation.module.io.earlyWakeUpCancel := memoryBlock.module.io.earlyWakeUpCancel(2)

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
  }
}
