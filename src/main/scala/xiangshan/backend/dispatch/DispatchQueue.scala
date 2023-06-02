package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import utils.HasPerfEvents
import xiangshan._
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, ParallelPriorityEncoder, ParallelPriorityMux, UIntToMask}

class DispatchQueueIO(enqNum: Int, deqNum: Int)(implicit p: Parameters) extends XSBundle {
  val enq = new Bundle {
    // output: dispatch queue can accept new requests
    val canAccept = Output(Bool())
    // input: need to allocate new entries (for address computing)
    val needAlloc = Vec(enqNum, Input(Bool()))
    // input: actually do the allocation (for write enable)
    val req = Vec(enqNum, Flipped(ValidIO(new MicroOp)))
  }
  val deq = Vec(deqNum, DecoupledIO(new MicroOp))
  val redirect = Flipped(ValidIO(new Redirect))
  val dqFull = Output(Bool())
}

class DispatchQueuePayload(entryNum:Int, enqNum:Int, deqNum:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val w = Input(Vec(enqNum, new Bundle{
      val en: Bool = Bool()
      val addrOH: UInt = UInt(entryNum.W)
      val data: MicroOp = new MicroOp
    }))
    val r = Vec(deqNum, new Bundle{
      val addrOH: UInt = Input(UInt(entryNum.W))
      val data: MicroOp = Output(new MicroOp)
    })
    val redirect: Valid[Redirect] = Input(Valid(new Redirect))
    val flushVec: UInt = Output(UInt(entryNum.W))
  })

  private val array = Reg(Vec(entryNum, new MicroOp))
  for(w <- io.w){
    for((hit, mem) <- w.addrOH.asBools.zip(array)){
      when(w.en && hit){
        mem := w.data
      }
    }
  }
  for(r <- io.r){
    r.data := Mux1H(r.addrOH, array)
  }

  private val redirectHits = array.map(_.robIdx.needFlush(io.redirect))
  io.flushVec := Cat(redirectHits.reverse)
}

class DispatchQueue (size: Int, enqNum: Int, deqNum: Int)(implicit p: Parameters)
  extends XSModule with HasCircularQueuePtrHelper with HasPerfEvents {
  val io: DispatchQueueIO = IO(new DispatchQueueIO(enqNum, deqNum))

  private class DispatchQueuePtr extends CircularQueuePtr[DispatchQueuePtr](size)

  private val payloadArray = Module(new DispatchQueuePayload(size, enqNum, deqNum))
  private val enqPtr = RegInit(0.U.asTypeOf(new DispatchQueuePtr)) //Fanout to enq logics and payloads
  private val enqPtrAux = RegInit(0.U.asTypeOf(new DispatchQueuePtr)) //Fanout to other logics
  private val deqPtr = RegInit(0.U.asTypeOf(new DispatchQueuePtr))
  private val validEntriesNum = distanceBetween(enqPtr, deqPtr)
  private val emptyEntriesNum = size.U - validEntriesNum
  io.dqFull := validEntriesNum === size.U

  private def SqueezeEnqueue(in:Vec[Valid[MicroOp]]):Vec[Valid[MicroOp]] = {
    val validMatrix = Wire(Vec(in.length + 1, Vec(in.length, Bool())))
    validMatrix.head.zip(in.map(_.valid)).foreach({case(a, b) => a := b})
    val dst = Wire(Vec(in.length, Valid(new MicroOp)))
    dst.zipWithIndex.foreach({case(o, idx) =>
      val validVec = validMatrix(idx).drop(idx)
      val selOH = ParallelPriorityMux(validVec, validVec.indices.map(i => (1 << (i + idx)).U(in.length.W)))
      validMatrix(idx + 1).zip(validMatrix(idx)).zip(selOH.asBools).foreach({ case ((n, p), s) =>
        n := p && (!s)
      })
      o.valid := validVec.reduce(_|_)
      o.bits := Mux1H(selOH, in.map(_.bits))
    })
    dst
  }
  //Make sure there is no bubble in enqueue data.
  private val squeezedEnqs = SqueezeEnqueue(io.enq.req)

  payloadArray.io.redirect := io.redirect
  private val enqMask = UIntToMask(enqPtrAux.value, size)
  private val deqMask = UIntToMask(deqPtr.value, size)
  private val enqXorDeq = enqMask ^ deqMask
  private val validsMask = Mux(deqPtr.value < enqPtrAux.value, enqXorDeq, (~enqXorDeq).asUInt)
  private val redirectMask = validsMask & payloadArray.io.flushVec
  private val flushNum = PopCount(redirectMask)

  io.enq.canAccept := (PopCount(io.enq.needAlloc) < emptyEntriesNum) || io.redirect.valid || RegNext(io.redirect.valid, false.B)
  for(idx <- 0 until enqNum){
    payloadArray.io.w(idx).en := squeezedEnqs(idx).valid
    payloadArray.io.w(idx).addrOH := UIntToOH((enqPtr + idx.U).value)
    payloadArray.io.w(idx).data := squeezedEnqs(idx).bits
  }
  private val actualEnqNum = PopCount(io.enq.req.map(_.valid))
  enqPtr := Mux(io.redirect.valid, enqPtr, enqPtr + actualEnqNum)
  enqPtrAux := Mux(io.redirect.valid, enqPtr, enqPtr + actualEnqNum)


  io.deq.zipWithIndex.foreach({case(deq, idx) =>
    deq.valid := (deqPtr + idx.U) < enqPtrAux
    payloadArray.io.r(idx).addrOH := UIntToOH((deqPtr + idx.U).value)
    deq.bits := payloadArray.io.r(idx).data
  })
  private val actualDeqNum = PopCount(io.deq.map(_.fire))
  deqPtr := Mux(io.redirect.valid, deqPtr + flushNum, deqPtr + actualDeqNum)

  assert(deqPtr <= enqPtrAux)
  assert(actualEnqNum <= emptyEntriesNum)
  assert(PopCount(squeezedEnqs.map(_.valid)) === actualEnqNum)
  for(i <- io.enq.req.indices){
    assert(Mux(i.U < actualEnqNum, squeezedEnqs(i).valid === true.B, squeezedEnqs(i).valid === false.B))
  }
  for(i <- 1 until squeezedEnqs.length){
    assert(Mux(squeezedEnqs(i).valid, squeezedEnqs(i).bits.robIdx > squeezedEnqs(i - 1).bits.robIdx, true.B))
  }
  assert(flushNum <= validEntriesNum)
  private val deqFlushNextMask = UIntToMask((deqPtr + flushNum).value, size)
  private val flushXorPresentMask = deqFlushNextMask ^ deqMask
  private val deqRollbackMask = Mux(deqPtr.value < (deqPtr + flushNum).value, flushXorPresentMask, ~flushXorPresentMask)
  assert(Mux(io.redirect.valid, deqRollbackMask === redirectMask, true.B), "Redirect mask should be continuous.")


  val perfEvents = Seq(
    ("dispatchq_in", actualEnqNum),
    ("dispatchq_out", actualDeqNum),
    ("dispatchq_out_try", PopCount(io.deq.map(_.valid))),
    ("dispatchq_1_4_valid ", validEntriesNum < (size / 4).U),
    ("dispatchq_2_4_valid ", validEntriesNum >= (size / 4).U && validEntriesNum <= (size / 2).U),
    ("dispatchq_3_4_valid ", validEntriesNum >= (size / 2).U && validEntriesNum <= (size * 3 / 4).U),
    ("dispatchq_4_4_valid ", validEntriesNum >= (size * 3 / 4).U)
  )
  generatePerfEvent()

}
