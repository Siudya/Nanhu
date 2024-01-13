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

package xiangshan.vector.vtyperename

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.vector._
import xiangshan.backend.rob._
import xs.utils._
import xiangshan.backend.execute.fu.FuOutput
import xiangshan.backend.execute.fu.csr.CSROpType
import xiangshan.backend.execute.fu.csr.vcsr.VCSRWithVtypeRenameIO

class VTypeEntry(implicit p: Parameters) extends VectorBaseBundle {
  val vill = Bool()
  val info = new VICsrInfo()
  val robIdx = new RobPtr
  val writebacked = Bool()
  val robEnqueued = Bool()
}

class VtpToVCtl(implicit p: Parameters) extends VectorBaseBundle {
  val psrc = Vec(3, UInt(PhyRegIdxWidth.W))
  val robIdx = new RobPtr
  val vcsrInfo = new VICsrInfo()
  val vtypeRdy = Bool()
  val vtypeIdx = UInt(VIVtypeRegsNum.W)
}

class VTypeRenameTable(size:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val w = Input(Vec(RenameWidth + 1, new Bundle{
      val en = Bool()
      val data = new VTypeEntry
      val addr = UInt(log2Ceil(size).W)
    }))
    val r = Vec(2, new Bundle{
      val addr = Input(UInt(log2Ceil(size).W))
      val data = Output(new VTypeEntry)
    })
    val vlUpdate = Input(Valid(UInt(log2Ceil(VLEN + 1).W)))
    val dispatchIn = Vec(RenameWidth, Input(Valid(new RobPtr)))
    val redirect = Input(Valid(new Redirect))
    val flushVec = Output(UInt(size.W))
  })
  private val headEntry = RegInit({
    val vtypeValue = 0.U.asTypeOf(new VTypeEntry)
    vtypeValue.vill := true.B
    vtypeValue.writebacked := true.B
    vtypeValue
  })
  private val tailEntries = Seq.fill(size - 1)(Reg(new VTypeEntry))
  private val table = headEntry +: tailEntries

  for((entry, idx) <- table.zipWithIndex){
    val hitSeq = io.w.map(w => w.en && w.addr === idx.U)
    val updateData = WireInit(entry)
    updateData.info := io.w.last.data.info
    updateData.vill := io.w.last.data.vill
    updateData.writebacked := io.w.last.data.writebacked
    val dataSeq = io.w.init.map(_.data) :+ updateData
    val data = Mux1H(hitSeq, dataSeq)
    val en = hitSeq.reduce(_|_)
    when(en) {
      entry := data
    }.elsewhen(io.vlUpdate.valid && io.vlUpdate.bits =/= 0.U){
      entry.info.vl := io.vlUpdate.bits
    }

    val dpInfoHit = io.dispatchIn.map(w => w.valid && w.bits === entry.robIdx).reduce(_ || _)
    when(dpInfoHit) {
      entry.robEnqueued := true.B
    }
  }

  for(r <- io.r){
    val rVec = table.indices.map(_.U === r.addr)
    r.data := Mux1H(rVec, table)
  }

  io.flushVec := Cat(table.map(e => e.robIdx.needFlush(io.redirect) || !e.robEnqueued).reverse)
}

class VtypeRename(implicit p: Parameters) extends VectorBaseModule with HasCircularQueuePtrHelper {

  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val robCommits = Flipped(new RobCommitIO)
    val needAlloc = Vec(RenameWidth, Input(Bool()))
    val canAccept = Output(Bool())
    val in = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
    val out = Vec(RenameWidth, ValidIO(new MicroOp))
    val toVCtl = Output(Vec(RenameWidth, new VtpToVCtl))
    val vcsr  = Flipped(new VCSRWithVtypeRenameIO)
    val vlUpdate = Input(Valid(UInt(log2Ceil(VLEN + 1).W)))
    val dispatchIn = Vec(RenameWidth, Input(Valid(new RobPtr)))
  })

  private val enqPtrInit = Wire(new VtypePtr)
  enqPtrInit.value := 1.U
  enqPtrInit.flag := 0.U
  private val table = Module(new VTypeRenameTable(VIVtypeRegsNum))
  private val enqPtr = RegInit(enqPtrInit)
  private val deqPtr = RegInit(0.U.asTypeOf(new VtypePtr))
  private val flushHeadPtr = RegInit(enqPtrInit)
  assert((deqPtr + 1.U) === flushHeadPtr)
  assert(enqPtr >= flushHeadPtr)
  dontTouch(enqPtr)

  class VtypePtr extends CircularQueuePtr[VtypePtr](VIVtypeRegsNum)

  private val validEntriesNum = distanceBetween(enqPtr, deqPtr)
  private val emptyEntriesNum = VIVtypeRegsNum.U - validEntriesNum
  private val emptyEntriesNumReg = RegInit((VIVtypeRegsNum - 1).U(log2Ceil(VIVtypeRegsNum + 1).W))
  assert(emptyEntriesNumReg === emptyEntriesNum)

  private val enqMask = UIntToMask(enqPtr.value, VIVtypeRegsNum)
  private val flushHeadMask = UIntToMask(flushHeadPtr.value, VIVtypeRegsNum)
  private val enqXorFlush = enqMask ^ flushHeadMask
  private val maybeFlushMask = Mux(flushHeadPtr.value <= enqPtr.value, enqXorFlush, (~enqXorFlush).asUInt)
  private val redirectMask = maybeFlushMask & table.io.flushVec
  private val flushNum = PopCount(redirectMask)

  private val allocNum = PopCount(io.needAlloc)

  private val setVlSeq = io.in.map(i => i.valid)
  private val realValids = setVlSeq.map(_ && io.canAccept)
  table.io.redirect := io.redirect

  table.io.w.last.en := io.vcsr.vtypeWbToRename.valid
  table.io.w.last.data.info := DontCare
  table.io.w.last.data.info.vma := io.vcsr.vtypeWbToRename.bits.vtype(7)
  table.io.w.last.data.info.vta := io.vcsr.vtypeWbToRename.bits.vtype(6)
  table.io.w.last.data.info.vsew := io.vcsr.vtypeWbToRename.bits.vtype(5, 3)
  table.io.w.last.data.info.vlmul := io.vcsr.vtypeWbToRename.bits.vtype(2, 0)
  table.io.w.last.data.info.vl := io.vcsr.vtypeWbToRename.bits.vl(7, 0)
  table.io.w.last.data.vill := io.vcsr.vtypeWbToRename.bits.vtype(8)
  table.io.w.last.data.info.vlmax := table.io.w.last.data.info.VLMAXGen()
  table.io.w.last.data.writebacked := true.B
  table.io.w.last.data.robIdx := DontCare
  table.io.w.last.addr := io.vcsr.vtypeWbToRename.bits.vtypeRegIdx

  table.io.vlUpdate := io.vlUpdate
  table.io.dispatchIn := io.dispatchIn

  private val enqAddrEnqSeq = Wire(Vec(RenameWidth, new VtypePtr))
  private val vtypeEnqSeq = Wire(Vec(RenameWidth, new VTypeEntry))
  private val enqAddrDeltas = Wire(Vec(RenameWidth, UInt(log2Ceil(RenameWidth).W)))
  enqAddrDeltas.zipWithIndex.foreach({case(d, i) =>
    if(i == 0){
      d := 0.U
    } else {
      d := PopCount(io.needAlloc.take(i))
    }
  })

  table.io.r(0).addr := (enqPtr - 1.U).value
  private val oldVType = WireInit(table.io.r(0).data)

  io.canAccept := allocNum <= emptyEntriesNumReg && !io.redirect.valid

  table.io.r(1).addr := deqPtr.value
  private val actualVl = Cat(Seq(
    0.U((XLEN - 8).W),
    table.io.r(1).data.info.vl
  ))
  private val actualVtype = Cat(Seq(
    table.io.r(1).data.vill,
    0.U((XLEN - 9).W),
    table.io.r(1).data.info.vma,
    table.io.r(1).data.info.vta,
    table.io.r(1).data.info.vsew,
    table.io.r(1).data.info.vlmul
  ))
  io.vcsr.vlRead.data.valid := io.vcsr.vlRead.readEn
  io.vcsr.vlRead.data.bits := actualVl
  io.vcsr.vtypeRead.data.valid := io.vcsr.vtypeRead.readEn
  io.vcsr.vtypeRead.data.bits := actualVtype
  io.vcsr.vtypeRead.data.valid := io.vcsr.vtypeRead.readEn
  io.vcsr.debug_vtype := actualVtype
  io.vcsr.debug_vl := actualVl


  /** ********************************************************************************************
   * 1.vsetivli
   * 2.vsetvli with src0 === x0 and dest =/= x0
   * 3.need old vl
   * 4.other vsetvl and vsetvli
   *
   * 1: fuOpType === vsetivli
   * 2: fuOpType === vsetvl && imm(19, 11).andR
   * 3: (fuOpType === vsetvli || fuOpType === vsetvl) && imm(19) && !imm(19, 11).andR
   *
   * 1, 2 should bypass to waitqueue; 3, 4 should wait for writeback from csr.
   * ******************************************************************************************* */

  private def GenVType(in:MicroOp):VTypeEntry = {
    val res = Wire(new VTypeEntry())
    res := DontCare
    res.writebacked := false.B
    res.robEnqueued := false.B
    res.robIdx := in.robIdx
    when(in.ctrl.fuOpType === CSROpType.vsetivli) {
      res.info.vma := in.ctrl.imm(12)
      res.info.vta := in.ctrl.imm(11)
      res.info.vsew := in.ctrl.imm(10, 8)
      res.info.vlmul := in.ctrl.imm(7, 5)
      res.info.vlmax := res.info.VLMAXGen()
      when(in.ctrl.imm(4, 0) > res.info.vlmax){
        res.info.vl := res.info.vlmax
      }.otherwise{
        res.info.vl := in.ctrl.imm(4, 0)
      }
      res.writebacked := true.B
    }.elsewhen(in.ctrl.fuOpType === CSROpType.vsetvli && in.ctrl.lsrc(0) === 0.U && in.ctrl.ldest =/= 0.U){
      res.info.vma := in.ctrl.imm(7)
      res.info.vta := in.ctrl.imm(6)
      res.info.vsew := in.ctrl.imm(5, 3)
      res.info.vlmul := in.ctrl.imm(2, 0)
      res.info.vlmax := res.info.VLMAXGen()
      res.info.vl := res.info.vlmax
      res.writebacked := true.B
    }
    res
  }

  setVlSeq.zipWithIndex.foreach({case(s, idx) =>
    val newVType = GenVType(io.in(idx).bits)
    enqAddrEnqSeq(idx) := enqPtr + enqAddrDeltas(idx)
    if(idx == 0){
      vtypeEnqSeq(idx) := Mux(s, newVType, oldVType)
    } else {
      vtypeEnqSeq(idx) := Mux(s, newVType, vtypeEnqSeq(idx - 1))
    }
  })


  private val uop = WireInit(io.in)
  private def NeedOldVl(in: MicroOp): Bool = {
    in.ctrl.lsrc(0) === 0.U && in.ctrl.ldest === 0.U && in.ctrl.fuOpType =/= CSROpType.vsetivli
  }
  private def SetToMax(in:MicroOp):Bool = {
    in.ctrl.lsrc(0) === 0.U && in.ctrl.ldest =/= 0.U && in.ctrl.fuOpType =/= CSROpType.vsetivli
  }

  for((u, i) <- uop.zipWithIndex) {
    val setToMax = SetToMax(u.bits)
    val needOldVl = NeedOldVl(u.bits)
    when(setToMax){
      u.bits.ctrl.imm := Cat(~(0.U(9.W)), io.in(i).bits.ctrl.imm(10, 0))
    }.elsewhen(needOldVl) {
      u.bits.ctrl.imm := Cat(1.U(1.W), 0.U(8.W), io.in(i).bits.ctrl.imm(10, 0))
    }
  }

  for((((w, addr), data), en) <- table.io.w.init.zip(enqAddrEnqSeq).zip(vtypeEnqSeq).zip(realValids)){
    w.en := en
    w.data := data
    w.addr := addr.value
  }

  private val actualEnqNum = PopCount(realValids)
  when(io.redirect.valid) {
    enqPtr := enqPtr - flushNum
  }.elsewhen(actualEnqNum =/= 0.U) {
    enqPtr := enqPtr + actualEnqNum
  }

  private val setVlCommSeq = io.robCommits.commitValid.zip(io.robCommits.info).map({case(a, b) => a && b.vtypeWb})
  private val setVlCommitted = io.robCommits.isCommit && setVlCommSeq.reduce(_|_)
  private val commmitNum = PopCount(setVlCommSeq)
  private val comValidReg = RegNext(setVlCommitted, false.B)
  private val comNumReg = RegEnable(commmitNum, setVlCommitted)
  when(comValidReg){
    deqPtr := deqPtr + comNumReg
    flushHeadPtr := flushHeadPtr + comNumReg
  }
  io.vcsr.vlUpdate.valid := RegNext(comValidReg, true.B)
  io.vcsr.vlUpdate.bits := actualVl

  private val actualFlushNum = Mux(io.redirect.valid, flushNum, 0.U)
  private val actualComNum = Mux(comValidReg, comNumReg, 0.U)
  emptyEntriesNumReg := (emptyEntriesNumReg +& actualFlushNum +& actualComNum) - actualEnqNum

  for (i <- 0 until RenameWidth) {
    io.toVCtl(i).psrc := DontCare
    io.toVCtl(i).robIdx := DontCare
    io.toVCtl(i).vcsrInfo := vtypeEnqSeq(i).info
    io.toVCtl(i).vtypeRdy := vtypeEnqSeq(i).writebacked
    io.toVCtl(i).vtypeIdx := (enqAddrEnqSeq(i) - 1.U).value

    io.out(i) := uop(i)
    io.out(i).bits.vtypeRegIdx := enqAddrEnqSeq(i).value
  }
}
