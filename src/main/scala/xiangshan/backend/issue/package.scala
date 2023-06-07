package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.rob.RobPtr
import xiangshan.{ExuInput, FuType, SrcState, SrcType, XSBundle}

package object issue {
  abstract class BasicStatusArrayEntry(val srcNum: Int)(implicit p: Parameters) extends XSBundle {
    val psrc: Vec[UInt] = Vec(srcNum, UInt(PhyRegIdxWidth.W))
    val pdest: UInt = UInt(PhyRegIdxWidth.W)
    val srcType: Vec[UInt] = Vec(srcNum, SrcType())
    val srcState: Vec[UInt] = Vec(srcNum, SrcState())
    val lpv: Vec[Vec[UInt]] = Vec(srcNum, Vec(loadUnitNum, UInt(LpvLength.W)))
    val fuType: UInt = FuType()
    val rfWen: Bool = Bool()
    val fpWen: Bool = Bool()
    val robIdx = new RobPtr
  }

  class BasicWakeupInfo(implicit p: Parameters) extends XSBundle {
    val pdest: UInt = UInt(PhyRegIdxWidth.W)
    val destType: UInt = SrcType()
    val robPtr = new RobPtr
  }

  class WakeUpInfo(implicit p: Parameters) extends BasicWakeupInfo {
    val lpv: Vec[UInt] = Vec(loadUnitNum, UInt(LpvLength.W))
  }

  class EarlyWakeUpInfo(implicit p: Parameters) extends BasicWakeupInfo {
    val lpv: UInt = UInt(LpvLength.W)
  }

  object RsType {
    def int: Int = 0

    def mem: Int = 1

    def fp: Int = 2

    def vec: Int = 3
  }

  case class RsParam
  (
    name: String,
    rsType: Int,
    entriesNum: Int = 48
  ) {
    //Unchangeable parameters
    val bankNum = 4
    require(entriesNum % bankNum == 0)
    val entryNumPerBank: Int = entriesNum / bankNum
    val isIntRs: Boolean = rsType == RsType.int
    val isMemRs: Boolean = rsType == RsType.mem
    val isFpRs: Boolean = rsType == RsType.fp
    val isVecRs: Boolean = rsType == RsType.vec
    val isLegal: Boolean = isIntRs || isMemRs || isFpRs || isVecRs

    def TypeName: String = {
      require(isLegal)
      if (isIntRs) {
        "Integer RS "
      } else if (isFpRs) {
        "Floating RS "
      } else if (isVecRs) {
        "Vector RS "
      } else {
        "Memory RS"
      }
    }
  }

  object RSFeedbackType {
    private val width = 5
    val tlbMiss: UInt = (1 << 3).U(width.W)
    val mshrFull: UInt = (1 << 3).U(width.W)
    val dataInvalid: UInt = (1 << 0).U(width.W)
    val bankConflict: UInt = (1 << 0).U(width.W)
    val ldVioCheckRedo: UInt = (1 << 0).U(width.W)
    val feedbackInvalid: UInt = (1 << 2).U(width.W)

    def apply(): UInt = UInt(width.W)
  }

  class RSFeedback(implicit p: Parameters) extends XSBundle {
    val rsIdx = new RsIdx
    val flushState: Bool = Bool()
    val sourceType: UInt = RSFeedbackType()
  }

  class RSFeedbackIO(implicit p: Parameters) extends XSBundle {
    // Note: you need to update in implicit Parameters p before imp MemRSFeedbackIO
    // for instance: MemRSFeedbackIO()(updateP)
    val feedbackSlow = ValidIO(new RSFeedback) // dcache miss queue full, dtlb miss
    val feedbackFast = ValidIO(new RSFeedback) // bank conflict
    val isFirstIssue: Bool = Input(Bool())
  }

  class IssueBundle(implicit p: Parameters) extends XSBundle {
    val issue = DecoupledIO(new ExuInput)
    val rsIdx: RsIdx = Output(new RsIdx)
    val rsFeedback: RSFeedbackIO = Flipped(new RSFeedbackIO)
  }

  class RsIdx(implicit p: Parameters) extends XSBundle {
    val bankIdxOH: UInt = UInt(coreParams.rsBankNum.W)
    val entryIdxOH: UInt = UInt((coreParams.maxRsEntryNum / coreParams.rsBankNum).W)
  }
}
