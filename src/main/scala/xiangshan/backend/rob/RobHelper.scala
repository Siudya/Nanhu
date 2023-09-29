package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import difftest._
import utils._
import xs.utils._
import xiangshan._
import xiangshan.frontend.FtqPtr
import xiangshan.backend.execute.exu.{ExuConfig, ExuType}
import xiangshan.backend.writeback._
import xiangshan.vector._
import xs.utils.perf.HasPerfLogging

/*  Commit Helper
*--------------------------------------------------------------------
*   1.manage tailPtr
*--------------------------------------------------------------------
*/

class RobCommitHelper(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper with HasPerfLogging {
  val io = IO(new Bundle {
    // for commits/flush
    val state = Input(UInt(2.W))
    val deq_valid = Vec(CommitWidth, Input(Bool()))
    val deq_writed = Vec(CommitWidth, Input(Bool()))
    val deq_isVec = Vec(CommitWidth, Input(Bool()))
    val exception_state = Flipped(ValidIO(new RobExceptionInfo))
    // for flush: when exception occurs, reset deqPtrs to range(0, CommitWidth)
    val intrBitSetReg = Input(Bool())
    val hasNoSpecExec = Input(Bool())
    val interrupt_safe = Input(Bool())
    val blockCommit = Input(Bool())
    // output: the CommitWidth deqPtr
    val deqPtrVec = Vec(CommitWidth, Output(new RobPtr))
    val deqPtrNextVec = Vec(CommitWidth, Output(new RobPtr))
    val commitValid = Vec(CommitWidth, Output(Bool()))
  })

  val vecMask = Wire(Vec(CommitWidth, Bool()))
  for((v, i) <- vecMask.zipWithIndex) {
    val vecNum = PopCount(io.deq_isVec.take(i+1))
    v := (vecNum === 0.U) || (vecNum === 1.U)
  }

  for((v, i) <- io.commitValid.zipWithIndex) {
    v := io.deq_valid(i) && io.deq_writed(i) && vecMask(i)
  }

  val deqPtrVec = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new RobPtr))))

  // for exceptions (flushPipe included) and interrupts:
  // only consider the first instruction
  val intrEnable      = io.intrBitSetReg && (!io.hasNoSpecExec) && io.interrupt_safe
  val exceptionEnable = io.deq_writed(0) && io.exception_state.valid && io.exception_state.bits.not_commit && (io.exception_state.bits.robIdx === deqPtrVec(0))

  val redirectOutValid = (io.state === 0.U) && io.deq_valid(0) && (intrEnable || exceptionEnable)

  // for normal commits: only to consider when there're no exceptions
  // we don't need to consider whether the first instruction has exceptions since it wil trigger exceptions.
  val commit_exception = io.exception_state.valid && !isAfter(io.exception_state.bits.robIdx, deqPtrVec.last)
  val canCommit = VecInit((0 until CommitWidth).map(i => io.deq_valid(i) && io.deq_writed(i) && vecMask(i)))
  val normalCommitCnt = PriorityEncoder(canCommit.map(c => !c) :+ true.B)
  // when io.intrBitSetReg or there're possible exceptions in these instructions,
  // only one instruction is allowed to commit
  val allowOnlyOne = commit_exception || io.intrBitSetReg
  val commitCnt = Mux(allowOnlyOne, canCommit(0), normalCommitCnt)

  val commitDeqPtrVec = VecInit(deqPtrVec.map(_ + commitCnt))
  val deqPtrVec_next = Mux((io.state === 0.U) && (!redirectOutValid) && (!io.blockCommit), commitDeqPtrVec, deqPtrVec)

  deqPtrVec := deqPtrVec_next

  io.deqPtrNextVec := deqPtrVec_next
  io.deqPtrVec      := deqPtrVec

  when (io.state === 0.U) {
    XSInfo(io.state === 0.U && commitCnt > 0.U, "retired %d insts\n", commitCnt)
  }
}

class RobEnqPtrWrapper(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // for input redirect
    val redirect = Input(Valid(new Redirect))
    // for enqueue
    val allowEnqueue = Input(Bool())
    val hasBlockBackward = Input(Bool())
    val enq = Vec(RenameWidth, Input(Bool()))
    val out = Output(Vec(RenameWidth, new RobPtr))
  })

  val enqPtrVec = RegInit(VecInit.tabulate(RenameWidth)(_.U.asTypeOf(new RobPtr)))

  // enqueue
  val canAccept = io.allowEnqueue && !io.hasBlockBackward
  val enqNum = Mux(canAccept, PopCount(io.enq), 0.U)

  for ((ptr, i) <- enqPtrVec.zipWithIndex) {
    when(io.redirect.valid) {
      ptr := Mux(io.redirect.bits.flushItself(), io.redirect.bits.robIdx + i.U, io.redirect.bits.robIdx + (i + 1).U)
    }.otherwise {
      ptr := ptr + enqNum
    }
  }

  io.out := enqPtrVec
}