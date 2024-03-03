package xiangshan.vector.vbackend.vissue.vrs

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{FuType, Redirect, SrcState, SrcType, XSBundle, XSModule}
import xiangshan.backend.issue._
import xiangshan.backend.rob.RobPtr
import xiangshan.vector.vbackend.vissue.vrs.VrsSelectInfo

class VrsStatusArrayEntry(implicit p: Parameters) extends XSBundle{
  val psrc: Vec[UInt] = Vec(4, UInt(PhyRegIdxWidth.W))
  val srcType: Vec[UInt] = Vec(4, SrcType())
  val srcState: Vec[UInt] = Vec(4, SrcState())
  val fuType: UInt = FuType()
  val robIdx: RobPtr = new RobPtr
  val uopIdx: UInt = UInt(3.W)
  val isOrdered: Bool = Bool()
}
class VrsIssueInfoGenerator(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val in = Input(Valid(new VrsStatusArrayEntry))
    val out = Output(Valid(new VrsSelectInfo))
  })
  private val iv = io.in.valid
  private val ib = io.in.bits
  private val readyToIssue = ib.srcState.map(_ === SrcState.rdy).reduce(_&_)
  io.out.valid := readyToIssue && iv
  io.out.bits.fuType := ib.fuType
  io.out.bits.robPtr := ib.robIdx
  io.out.bits.uopIdx := ib.uopIdx
  io.out.bits.isOrdered := ib.isOrdered
}

class VrsStatusArrayEntryUpdateNetwork(issueWidth:Int, wakeupWidth:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val entry = Input(Valid(new VrsStatusArrayEntry))
    val entryNext = Output(Valid(new VrsStatusArrayEntry))
    val updateEnable = Output(Bool())
    val enq = Input(Valid(new VrsStatusArrayEntry))
    val issue = Input(Vec(issueWidth, Bool()))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
    val redirect = Input(Valid(new Redirect))
  })

  private val miscNext = WireInit(io.entry)
  private val enqNext = Wire(Valid(new VrsStatusArrayEntry))
  private val enqUpdateEn = WireInit(false.B)

  //Start of wake up
  private val pregMatch = io.entry.bits.psrc
    .zip(io.entry.bits.srcType)
    .map(p => io.wakeup.map(elm =>
      elm.bits.pdest === p._1 && elm.valid && p._2 === elm.bits.destType && !(p._1 === 0.U && p._2 === SrcType.reg)
    ))
  for((n, v) <- miscNext.bits.srcState zip pregMatch){
    val shouldUpdateSrcState = Cat(v).orR
    when(shouldUpdateSrcState){
      n := SrcState.rdy
    }
  }
  pregMatch.foreach(hv => when(io.entry.valid){assert(PopCount(hv) <= 1.U)})
  private val miscUpdateEnWakeUp = pregMatch.map(_.reduce(_|_)).reduce(_|_)
  //End of wake up

  //Start of issue
  private val shouldBeIssued = Cat(io.issue).orR
  private val srcAllReady = io.entry.bits.srcState.map(_ === SrcState.rdy).reduce(_&_)
  when(shouldBeIssued){assert(io.entry.valid && srcAllReady)}
  //End of issue and cancel

  //Start of dequeue and redirect
  private val shouldBeFlushed = io.entry.valid & io.entry.bits.robIdx.needFlush(io.redirect)
  when(shouldBeIssued || shouldBeFlushed) {
    miscNext.valid := false.B
  }
  //End of dequeue and redirect

  //Start of Enqueue
  enqNext.bits := io.enq.bits
  enqNext.valid := io.enq.valid
  enqUpdateEn := enqNext.valid
  //End of Enqueue

  io.updateEnable := Mux(io.entry.valid, miscUpdateEnWakeUp | srcAllReady | shouldBeFlushed, enqUpdateEn)
  io.entryNext := Mux(enqUpdateEn, enqNext, miscNext)

  private val debugTimeoutCnt = RegInit(0.U(16.W))
  when(io.enq.valid) {
    debugTimeoutCnt := 0.U
  }.elsewhen(io.entry.valid) {
    debugTimeoutCnt := debugTimeoutCnt + 1.U
  }
  assert(debugTimeoutCnt < 200000.U, "Uop is not dequeued for 200000 cycles!")
}

class VrsStatusArray(entryNum:Int, issueWidth:Int, wakeupWidth:Int, loadUnitNum:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))

    val selectInfo = Output(Vec(entryNum, Valid(new VrsSelectInfo)))
    val allocateInfo = Output(UInt(entryNum.W))

    val enq = Input(Valid(new Bundle{
      val addrOH = UInt(entryNum.W)
      val data = new VrsStatusArrayEntry
    }))

    val issue = Input(Vec(issueWidth, Valid(UInt(entryNum.W))))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
  })

  private val statusArray = Reg(Vec(entryNum, new VrsStatusArrayEntry))
  private val statusArrayValid = RegInit(VecInit(Seq.fill(entryNum)(false.B)))
  private val statusArrayValid_dup = RegInit(VecInit(Seq.fill(entryNum)(false.B)))

  //Start of select logic
  for(((selInfo, saEntry), saValid) <- io.selectInfo
    .zip(statusArray)
    .zip(statusArrayValid)){
    val entryToSelectInfoCvt = Module(new VrsIssueInfoGenerator)
    entryToSelectInfoCvt.io.in.valid := saValid
    entryToSelectInfoCvt.io.in.bits := saEntry
    selInfo := entryToSelectInfoCvt.io.out
  }
  //End of select logic

  //Start of allocate logic
  io.allocateInfo := Cat(statusArrayValid_dup.reverse)
  //End of allocate logic

  for((((v, va), d), idx) <- statusArrayValid
    .zip(statusArrayValid_dup)
    .zip(statusArray)
    .zipWithIndex
      ){
    val updateNetwork = Module(new VrsStatusArrayEntryUpdateNetwork(issueWidth, wakeupWidth))
    updateNetwork.io.entry.valid := v
    updateNetwork.io.entry.bits := d
    updateNetwork.io.enq.valid := io.enq.valid & io.enq.bits.addrOH(idx)
    updateNetwork.io.enq.bits := io.enq.bits.data
    updateNetwork.io.issue := VecInit(io.issue.map(i => i.valid & i.bits(idx)))
    updateNetwork.io.wakeup := io.wakeup
    updateNetwork.io.redirect := io.redirect

    val en = updateNetwork.io.updateEnable
    when(en) {
      v  := updateNetwork.io.entryNext.valid
      va := updateNetwork.io.entryNext.valid
      d  := updateNetwork.io.entryNext.bits
    }
  }

  assert(Cat(statusArrayValid) === Cat(statusArrayValid_dup))
  when(io.enq.valid){assert(PopCount(io.enq.bits.addrOH) === 1.U)}
  assert((Mux(io.enq.valid, io.enq.bits.addrOH, 0.U) & Cat(statusArrayValid.reverse)) === 0.U)
  for(iss <- io.issue){
    when(iss.valid){assert(PopCount(iss.bits & Cat(statusArrayValid.reverse)) === 1.U)}
  }
}
