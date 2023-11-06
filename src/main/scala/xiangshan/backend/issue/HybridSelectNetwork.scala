package xiangshan.backend.issue
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.exu.ExuConfig
import xiangshan.backend.rob.RobPtr
import xiangshan.{Redirect, XSBundle, XSModule}
import xs.utils.{LogicShiftRight, ParallelOperationN}

class HybridSelectInfo(implicit p: Parameters) extends XSBundle {
  val robPtr = new RobPtr
  val lpv = Vec(loadUnitNum, UInt(LpvLength.W))
}

class PrioritySelectPolicy(inputNum:Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in = Input(Vec(inputNum, Valid(new HybridSelectInfo)))
    val out = Output(Valid(UInt(inputNum.W)))
  })
  private val lpvMask = Cat(io.in.map(in => Cat(in.bits.lpv).orR).reverse)
  private val noLpvMask = Cat(io.in.map(in => Cat(in.bits.lpv).orR).map(!_).reverse)
  private val validMask = Cat(io.in.map(_.valid).reverse)

  private val selMask = Mux((validMask & noLpvMask).orR, validMask & noLpvMask, validMask & lpvMask)
  io.out.valid := validMask.orR
  io.out.bits := PriorityEncoderOH(selMask)
}

class OldestSelectPolicy(inputNum:Int, haveEqual:Boolean)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in = Input(Vec(inputNum, Valid(new HybridSelectInfo)))
    val out = Output(Valid(UInt(inputNum.W)))
  })
  private def ReductionFunc(in: Seq[(Valid[HybridSelectInfo], UInt)]):(Valid[HybridSelectInfo], UInt) = {
    val selectPolicy = Module(new SelectPolicy(in.length, true, haveEqual))
    val interRes = Wire(Valid(new HybridSelectInfo))
    selectPolicy.io.in.zip(in).foreach({case(a, b) =>
      a.valid := b._1.valid
      a.bits := b._1.bits.robPtr
    })
    interRes.valid := selectPolicy.io.out.valid
    interRes.bits := Mux1H(selectPolicy.io.out.bits, in.map(_._1.bits))
    val idx = Mux1H(selectPolicy.io.out.bits, in.map(_._2))
    (interRes, idx)
  }

  private val res = ParallelOperationN(io.in.zipWithIndex.map(in => (in._1, (1L << in._2).U(inputNum.W))), 6, ReductionFunc)
  io.out.valid := res._1.valid
  io.out.bits := res._2
}

class HybridSelectNetwork(bankNum:Int, entryNum:Int, issueNum:Int, val cfg:ExuConfig, haveEqual:Boolean, name:Option[String] = None)(implicit p: Parameters) extends XSModule {
  require(issueNum <= bankNum && 0 < issueNum && bankNum % issueNum == 0, "Illegal number of issue ports are supported now!")
  private val fuTypeList = cfg.fuConfigs.map(_.fuType)
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))
    val selectInfo = Input(Vec(bankNum, Vec(entryNum, Valid(new SelectInfo))))
    val issueInfo = Vec(issueNum, Decoupled(new SelectResp(bankNum, entryNum)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })
  override val desiredName: String = name.getOrElse("SelectNetwork")
  private val finalSelectResult = Wire(Vec(issueNum, Valid(new SelectResp(bankNum, entryNum))))

  private val selectInputPerBank = io.selectInfo.zipWithIndex.map({ case (si, bidx) =>
    si.zipWithIndex.map({ case (in, eidx) =>
      val selInfo = Wire(Valid(new SelectResp(bankNum, entryNum)))
      selInfo.valid := in.valid && fuTypeList.map(_ === in.bits.fuType).reduce(_ | _)
      selInfo.bits.info := in.bits
      selInfo.bits.bankIdxOH := (1 << bidx).U(bankNum.W)
      selInfo.bits.entryIdxOH := (1 << eidx).U(entryNum.W)
      selInfo
    })
  })
  private val selLen = bankNum / issueNum
  private val selectInputPerIssue = selectInputPerBank.grouped(selLen).map(_.flatten).toSeq
  for(i <- 0 until issueNum) {
    val inSeq = selectInputPerIssue(i)
    val iss = finalSelectResult(i)
    val pSelector = Module(new PrioritySelectPolicy(inSeq.length))
    val oSelector = Module(new OldestSelectPolicy(inSeq.length, haveEqual))
    pSelector.io.in.zip(inSeq).foreach({case(a, b) =>
      a.valid := b.valid
      a.bits.robPtr := b.bits.info.robPtr
      a.bits.lpv := b.bits.info.lpv
    })
    oSelector.io.in.zip(inSeq).foreach({ case (a, b) =>
      a.valid := b.valid & !(iss.valid & iss.bits.entryIdxOH === b.bits.entryIdxOH & iss.bits.bankIdxOH === b.bits.bankIdxOH)
      a.bits.robPtr := b.bits.info.robPtr
      a.bits.lpv := b.bits.info.lpv
    })
    val pSel = pSelector.io.out
    val oSelV = RegNext(oSelector.io.out.valid, false.B)
    val oSelB = RegEnable(oSelector.io.out.bits, oSelector.io.out.valid)
    val oSel = Wire(Valid(UInt(inSeq.length.W)))
    oSel.valid := oSelV & (oSelB & Cat(inSeq.map(_.valid).reverse)).orR
    oSel.bits := oSelB
    val selOH = Mux(oSel.valid, oSel.bits, pSel.bits)

    finalSelectResult(i).valid := pSel.valid | oSel.valid
    finalSelectResult(i).bits := Mux1H(selOH, inSeq.map(_.bits))
  }

  for ((outPort, driver) <- io.issueInfo.zip(finalSelectResult)) {
    val cancelCond = driver.bits.info.lpv.zip(io.earlyWakeUpCancel).map({case(l, c) => l(0) & c}).reduce(_|_)
    outPort.valid := driver.valid & !cancelCond & !io.redirect.valid
    outPort.bits.bankIdxOH := driver.bits.bankIdxOH
    outPort.bits.entryIdxOH := driver.bits.entryIdxOH
    outPort.bits.info := driver.bits.info
    outPort.bits.info.lpv.zip(driver.bits.info.lpv).foreach({ case (o, i) => o := LogicShiftRight(i, 1)})
  }
}
