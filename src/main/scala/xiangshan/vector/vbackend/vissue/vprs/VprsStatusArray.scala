package xiangshan.vector.vbackend.vissue.vprs

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{FuType, MicroOp, Redirect, SrcState, SrcType, XSBundle, XSModule}
import xiangshan.backend.issue._
import xiangshan.backend.rob.RobPtr
import xs.utils.PickOneLow
class VprsStatusArrayEntry(implicit p: Parameters) extends XSBundle{
  val robPtr: RobPtr = new RobPtr
  val prs: UInt = UInt(PhyRegIdxWidth.W)
  val pvs1: Vec[UInt] = Vec(8, UInt(PhyRegIdxWidth.W))
  val pvs2: Vec[UInt] = Vec(8, UInt(PhyRegIdxWidth.W))
  val pov: Vec[UInt] = Vec(8, UInt(PhyRegIdxWidth.W))
  val pvm: UInt = UInt(PhyRegIdxWidth.W)
  val prsType: UInt = SrcType()
  val prsState: UInt = SrcState()
  val pvs1States: Vec[UInt] = Vec(8, SrcState())
  val pvs2States: Vec[UInt] = Vec(8, SrcState())
  val povStates: Vec[UInt] = Vec(8, SrcState())
  val pvmState: UInt = SrcState()
  val allMerged: Bool = Bool()
  val uopValids: Vec[Bool] = Vec(8, Bool())
}

class VprsStatusArrayEntryUpdateNetwork(sWkpWidth:Int, vWkpWidth:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val enq = Input(Valid(new MicroOp))
    val enqIsMerge = Input(Bool())
    val entry = Input(Valid(new VprsStatusArrayEntry))
    val issued = Input(Bool())
    val scalarWakeUps = Input(Vec(sWkpWidth, Valid(new WakeUpInfo)))
    val vectorWakeUps = Input(Vec(vWkpWidth, Valid(new WakeUpInfo)))
    val redirect = Input(Valid(new Redirect))

    val entryNext = Output(Valid(new VprsStatusArrayEntry))
    val updateEnable = Output(Bool())
  })

  private val entryNext = WireInit(io.entry)
  when(io.enq.valid){
    val agnostic = io.enq.bits.vCsrInfo.vta(0) && Mux(io.enq.bits.vctrl.vm, io.enq.bits.vCsrInfo.vma(0), true.B)
    val isSlideUp = io.enq.bits.vctrl.funct6 === "b001110".U
    val dontCareDest = agnostic && !isSlideUp
    when(io.enqIsMerge){
      assert(io.entry.valid)
      entryNext.bits.pvs1(io.enq.bits.uopIdx) := io.enq.bits.psrc(0)
      entryNext.bits.pvs1States(io.enq.bits.uopIdx) := Mux(SrcType.isVec(io.enq.bits.ctrl.srcType(0)), io.enq.bits.srcState(0), SrcState.rdy)
      entryNext.bits.pvs2(io.enq.bits.uopIdx) := io.enq.bits.psrc(1)
      entryNext.bits.pvs2States(io.enq.bits.uopIdx) := io.enq.bits.srcState(1)
      entryNext.bits.pov(io.enq.bits.uopIdx) := io.enq.bits.psrc(2)
      entryNext.bits.povStates(io.enq.bits.uopIdx) := Mux(dontCareDest, SrcState.rdy, io.enq.bits.srcState(2))
      entryNext.bits.uopValids(io.enq.bits.uopIdx) := true.B
      entryNext.bits.allMerged := (io.enq.bits.uopNum - 1.U) === io.enq.bits.uopIdx
    }.otherwise{
      assert(!io.entry.valid)
      val src1IsVec = SrcType.isVec(io.enq.bits.ctrl.srcType(0))
      entryNext.bits.robPtr := io.enq.bits.robIdx
      entryNext.bits.prs := io.enq.bits.psrc(0)
      entryNext.bits.prsType := io.enq.bits.ctrl.srcType(0)
      entryNext.bits.prsState := Mux(SrcType.isRegOrFp(io.enq.bits.ctrl.srcType(0)), io.enq.bits.srcState(0), SrcState.rdy)
      entryNext.bits.pvs1(0) := io.enq.bits.psrc(0)
      entryNext.bits.pvs1States(0) := Mux(SrcType.isVec(io.enq.bits.ctrl.srcType(0)), io.enq.bits.srcState(0), SrcState.rdy)
      entryNext.bits.pvs2(0) := io.enq.bits.psrc(1)
      entryNext.bits.pvs2States(0) := io.enq.bits.srcState(1)
      entryNext.bits.pov(0) := io.enq.bits.psrc(2)
      entryNext.bits.povStates(0) := Mux(dontCareDest, SrcState.rdy, io.enq.bits.srcState(2))
      entryNext.bits.pvm := io.enq.bits.vm
      entryNext.bits.pvmState := Mux(io.enq.bits.vctrl.vm, io.enq.bits.vmState, SrcState.rdy)
      entryNext.bits.allMerged := io.enq.bits.uopNum === 1.U
      entryNext.bits.uopValids(0) := true.B
      entryNext.valid := true.B

      entryNext.bits.pvs1States.zipWithIndex.drop(1).foreach({ case (s,i) =>
        s := Mux(src1IsVec, Mux(i.U < io.enq.bits.uopNum, SrcState.busy, SrcState.rdy), SrcState.rdy)
      })
      entryNext.bits.pvs2States.zipWithIndex.drop(1).foreach({ case (s, i) =>
        s := Mux(i.U < io.enq.bits.uopNum, SrcState.busy, SrcState.rdy)
      })
      entryNext.bits.povStates.zipWithIndex.drop(1).foreach({ case (s, i) =>
        s := Mux(i.U < io.enq.bits.uopNum, Mux(agnostic, SrcState.rdy, SrcState.busy), SrcState.rdy)
      })
      entryNext.bits.uopValids.drop(1).foreach(_ := false.B)
    }
  }

  private val rsWakeupValid = io.scalarWakeUps.map(wkp=> {
    io.entry.valid && wkp.valid && wkp.bits.destType === io.entry.bits.prsType &&
      io.entry.bits.prs === wkp.bits.pdest &&
      !(SrcType.isReg(io.entry.bits.prsType) && io.entry.bits.prs === 0.U)
  }).reduce(_|_)
  when(rsWakeupValid){
    assert(io.entry.bits.prsState === SrcState.busy)
    entryNext.bits.prsState := SrcState.rdy
  }

  private val pvsSeq = io.entry.bits.pvs1 ++ io.entry.bits.pvs2 ++ io.entry.bits.pov :+ io.entry.bits.pvm
  private val pvsStateSeq = io.entry.bits.pvs1States ++ io.entry.bits.pvs2States ++ io.entry.bits.povStates :+ io.entry.bits.pvmState
  private val pvsStateNextSeq = entryNext.bits.pvs1States ++ entryNext.bits.pvs2States ++ entryNext.bits.povStates :+ entryNext.bits.pvmState
  private val uvSeq = io.entry.bits.uopValids ++ io.entry.bits.uopValids ++ io.entry.bits.uopValids :+ true.B
  private val pvsWkpHitsSeq = pvsSeq.zip(pvsStateSeq).zip(uvSeq).map({case((pv, st), uv) =>
    io.vectorWakeUps.map(wkp => {
      io.entry.valid && uv && wkp.valid && pv === wkp.bits.pdest && st === SrcState.busy && wkp.bits.destType === SrcType.vec
    }).reduce(_|_)
  })
  pvsStateNextSeq.zip(pvsWkpHitsSeq).foreach({case(s, w) => when(w){s := SrcState.rdy}})
  private val wkpUpdateEn = pvsWkpHitsSeq.reduce(_|_) || rsWakeupValid

  private val flushed = io.entry.bits.robPtr.needFlush(io.redirect)
  when(flushed || io.issued) {
    entryNext.valid := false.B
  }

  io.entryNext := entryNext
  io.updateEnable := io.enq.valid || io.entry.valid && (io.issued || flushed || wkpUpdateEn)

  private val debugTimeoutCnt = RegInit(0.U(16.W))
  when(io.enq.valid) {
    debugTimeoutCnt := 0.U
  }.elsewhen(io.entry.valid) {
    debugTimeoutCnt := debugTimeoutCnt + 1.U
  }
  assert(debugTimeoutCnt < 200000.U, "Uop is not dequeued for 200000 cycles!")
}

class VprsStatusArray(sWkpWidth:Int, vWkpWidth:Int)(implicit p: Parameters) extends XSModule{
  private val size = vectorParameters.vPRsDepth
  val io = IO(new Bundle{
    val enq = Flipped(Decoupled(new MicroOp()))
    val selectInfo = Output(Vec(size, Valid(new VprsStatusArrayEntry)))
    val issueOH = Input(Valid(UInt(size.W)))
    val enqToPayload = Output(Valid(UInt(size.W)))
    val pdestUpdate = Output(Valid(UInt(size.W)))
    val scalarWakeUps = Input(Vec(sWkpWidth, Valid(new WakeUpInfo)))
    val vectorWakeUps = Input(Vec(vWkpWidth, Valid(new WakeUpInfo)))
    val redirect = Input(Valid(new Redirect))
  })

  private val array = Reg(Vec(size, new VprsStatusArrayEntry))
  private val valids = RegInit(VecInit(Seq.fill(size)(false.B)))
  private val validsAux = RegInit(VecInit(Seq.fill(size)(false.B)))

  for(((port, entry), v) <- io.selectInfo.zip(array).zip(valids)){
    val selCond0 = entry.prsState === SrcState.rdy
    val selCond1 = entry.pvs1States.map(_ === SrcState.rdy).reduce(_ && _)
    val selCond2 = entry.pvs2States.map(_ === SrcState.rdy).reduce(_ && _)
    val selCond3 = entry.povStates.map(_ === SrcState.rdy).reduce(_ && _)
    val selCond4 = entry.pvmState === SrcState.rdy
    port.valid := v && selCond0 && selCond1 && selCond2 && selCond3 && selCond4 && entry.allMerged
    port.bits := entry
  }

  private val mergeVec = valids.zip(array).map({case(v, e) =>
    v && e.robPtr === io.enq.bits.robIdx
  })

  when(io.enq.valid){assert(PopCount(mergeVec) <= 1.U)}

  private val emptyEntry = PickOneLow(validsAux)

  private val needMerge = mergeVec.reduce(_|_)
  private val needAlloc = !needMerge && emptyEntry.valid && io.enq.valid

  io.enq.ready := needMerge | emptyEntry.valid
  private val enqEntryOH = Mux(needMerge, Cat(mergeVec.reverse), emptyEntry.bits)

  io.enqToPayload.valid := needAlloc
  io.enqToPayload.bits := emptyEntry.bits
  io.pdestUpdate.valid := io.enq.fire
  io.pdestUpdate.bits := enqEntryOH

  for(((v, e), i) <- valids.zip(array).zipWithIndex){
    val updateNetwork = Module(new VprsStatusArrayEntryUpdateNetwork(sWkpWidth, vWkpWidth))
    updateNetwork.io.enq.valid := io.enq.fire & enqEntryOH(i)
    updateNetwork.io.enq.bits := io.enq.bits
    updateNetwork.io.enqIsMerge := needMerge
    updateNetwork.io.entry.valid := v
    updateNetwork.io.entry.bits := e
    updateNetwork.io.issued := io.issueOH.valid && io.issueOH.bits(i)
    updateNetwork.io.scalarWakeUps := io.scalarWakeUps
    updateNetwork.io.vectorWakeUps := io.vectorWakeUps
    updateNetwork.io.redirect := io.redirect
    when(updateNetwork.io.updateEnable){
      v := updateNetwork.io.entryNext.valid
      validsAux(i) := updateNetwork.io.entryNext.valid
      e := updateNetwork.io.entryNext.bits
    }
  }
  assert(valids === validsAux)
  when(io.issueOH.valid){assert((valids.asUInt & io.issueOH.bits).orR)}
}
