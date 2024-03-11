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
package xiangshan.frontend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.frontend.icache._
import utils._
import xs.utils._
import scala.{Tuple2 => &}


class FetchRequestBundle(implicit p: Parameters) extends XSBundle with HasICacheParameters {

  //fast path: Timing critical
  val startAddr       = UInt(VAddrBits.W)
  val nextlineStart   = UInt(VAddrBits.W)
  val nextStartAddr   = UInt(VAddrBits.W)
  //slow path
  val ftqIdx          = new FtqPtr
  val ftqOffset       = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))

  def crossCacheline =  startAddr(blockOffBits - 1) === 1.U

  def fromFtqPcBundle(b: FtqPCEntry) = {
    this.startAddr := b.startAddr
    this.nextlineStart := b.nextLineAddr
    when (b.fallThruError) {
      val nextBlockHigherTemp = Mux(startAddr(log2Ceil(PredictWidth)+instOffsetBits), b.startAddr, b.nextLineAddr)
      val nextBlockHigher = nextBlockHigherTemp(VAddrBits-1, log2Ceil(PredictWidth)+instOffsetBits+1)
      this.nextStartAddr :=
        Cat(nextBlockHigher,
          startAddr(log2Ceil(PredictWidth)+instOffsetBits) ^ 1.U(1.W),
          startAddr(log2Ceil(PredictWidth)+instOffsetBits-1, instOffsetBits),
          0.U(instOffsetBits.W)
        )
    }
    this
  }
  override def toPrintable: Printable = {
    p"[start] ${Hexadecimal(startAddr)} [next] ${Hexadecimal(nextlineStart)}" +
      p"[tgt] ${Hexadecimal(nextStartAddr)} [ftqIdx] $ftqIdx [jmp] v:${ftqOffset.valid}" +
      p" offset: ${ftqOffset.bits}\n"
  }
}

class FtqICacheInfo(implicit p: Parameters)extends XSBundle with HasICacheParameters{
  val startAddr           = UInt(VAddrBits.W)
  val nextlineStart       = UInt(VAddrBits.W)
  def crossCacheline =  startAddr(blockOffBits - 1) === 1.U
  def fromFtqPcBundle(b: FtqPCEntry) = {
    this.startAddr := b.startAddr
    this.nextlineStart := b.nextLineAddr
    this
  }
}

class IFUICacheIO(implicit p: Parameters)extends XSBundle with HasICacheParameters{
  val icacheReady       = Output(Bool())
  val resp              = Vec(PortNumber, ValidIO(new ICacheMainPipeResp))
}

class FtqToICacheRequestBundle(implicit p: Parameters)extends XSBundle with HasICacheParameters{
  val pcMemRead           = Vec(5, new FtqICacheInfo)
  val readValid           = Vec(5, Bool())
}


class PredecodeWritebackBundle(implicit p:Parameters) extends XSBundle {
  val pc           = Vec(PredictWidth, UInt(VAddrBits.W))
  val pd           = Vec(PredictWidth, new PreDecodeInfo) // TODO: redefine Predecode
  val ftqIdx       = new FtqPtr
  val ftqOffset    = UInt(log2Ceil(PredictWidth).W)
  val misOffset    = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val cfiOffset    = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val target       = UInt(VAddrBits.W)
  val jalTarget    = UInt(VAddrBits.W)
  val instrRange   = Vec(PredictWidth, Bool())
}

// Ftq send req to Prefetch
class PrefetchRequest(implicit p:Parameters) extends XSBundle {
  val target          = UInt(VAddrBits.W)
}

class FtqPrefechBundle(implicit p:Parameters) extends XSBundle {
  val req = DecoupledIO(new PrefetchRequest)
}

class mmioCommitRead(implicit p: Parameters) extends XSBundle {
  val mmioFtqPtr = Output(new FtqPtr)
  val mmioLastCommit = Input(Bool())
}

class FetchToIBuffer(implicit p: Parameters) extends XSBundle {
  val instrs    = Vec(PredictWidth, UInt(32.W))
  val valid     = UInt(PredictWidth.W)
  val enqEnable = UInt(PredictWidth.W)
  val pd        = Vec(PredictWidth, new PreDecodeInfo)
  val pc        = Vec(PredictWidth, UInt(VAddrBits.W))
  val foldpc    = Vec(PredictWidth, UInt(MemPredPCWidth.W))
  val ftqPtr       = new FtqPtr
  val ftqOffset    = Vec(PredictWidth, ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
  val ipf          = Vec(PredictWidth, Bool())
  val acf          = Vec(PredictWidth, Bool())
  val crossPageIPFFix = Vec(PredictWidth, Bool())
  val triggered    = Vec(PredictWidth, new TriggerCf)
  val mmioFetch = Bool()
}

// class BitWiseUInt(val width: Int, val init: UInt) extends Module {
//   val io = IO(new Bundle {
//     val set
//   })
// }
// Move from BPU
abstract class GlobalHistory(implicit p: Parameters) extends XSBundle with HasBPUConst {
  def update(brValid: Vec[Bool], real_taken_mask: Vec[Bool]): GlobalHistory
}

class ShiftingGlobalHistory(implicit p: Parameters) extends GlobalHistory {
  val predHist = UInt(HistoryLength.W)

  def update(shift: UInt, taken: Bool, hist: UInt = this.predHist): ShiftingGlobalHistory = {
    val g = Wire(new ShiftingGlobalHistory)
    g.predHist := (hist << shift) | taken
    g
  }

  def update(brValid: Vec[Bool], real_taken_mask: Vec[Bool]): ShiftingGlobalHistory = {
    require(brValid.length == numBr)
    require(real_taken_mask.length == numBr)
    val last_valid_idx = PriorityMux(
      brValid.reverse :+ true.B,
      (numBr to 0 by -1).map(_.U(log2Ceil(numBr+1).W))
    )
    val first_taken_idx = PriorityEncoder(false.B +: real_taken_mask)
    val smaller = Mux(last_valid_idx < first_taken_idx,
      last_valid_idx,
      first_taken_idx
    )
    val shift = smaller
    val taken = real_taken_mask.reduce(_||_)
    update(shift, taken, this.predHist)
  }

  // static read
  def read(n: Int): Bool = predHist.asBools(n)

  final def === (that: ShiftingGlobalHistory): Bool = {
    predHist === that.predHist
  }

  final def =/= (that: ShiftingGlobalHistory): Bool = !(this === that)
}

// circular global history pointer
class CGHPtr(implicit p: Parameters) extends CircularQueuePtr[CGHPtr](
  p => p(XSCoreParamsKey).HistoryLength
){
}

object CGHPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): CGHPtr = {
    val ptr = Wire(new CGHPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: CGHPtr)(implicit p: Parameters): CGHPtr = {
    apply(!ptr.flag, ptr.value)
  }
}

class CircularGlobalHistory(implicit p: Parameters) extends GlobalHistory {
  val buffer = Vec(HistoryLength, Bool())
  type HistPtr = UInt
  def update(brValid: Vec[Bool], real_taken_mask: Vec[Bool]): CircularGlobalHistory = {
    this
  }
}

class FoldedHistory(val len: Int, val compLen: Int, val max_update_num: Int)(implicit p: Parameters)
  extends XSBundle with HasBPUConst {
  require(compLen >= 1)
  require(len > 0)
  // require(folded_len <= len)
  require(compLen >= max_update_num)
  val foldedHist = UInt(compLen.W)

  def need_oldest_bits = len > compLen
  def info = (len, compLen)
  def oldest_bit_to_get_from_ghr = (0 until max_update_num).map(len - _ - 1)
  def oldest_bit_pos_in_folded = oldest_bit_to_get_from_ghr map (_ % compLen)
  def oldest_bit_wrap_around = oldest_bit_to_get_from_ghr map (_ / compLen > 0)
  def oldest_bit_start = oldest_bit_pos_in_folded.head

  def get_oldest_bits_from_ghr(ghr: Vec[Bool], histPtr: CGHPtr) = {
    // TODO: wrap inc for histPtr value
    oldest_bit_to_get_from_ghr.map(i => ghr((histPtr + (i+1).U).value))
  }

  def circular_shift_left(src: UInt, shamt: Int) = {
    val srcLen = src.getWidth
    val src_doubled = Cat(src, src)
    val shifted = src_doubled(srcLen*2-1-shamt, srcLen-shamt)
    shifted
  }

  // slow path, read bits from ghr
  def update(ghr: Vec[Bool], histPtr: CGHPtr, num: Int, taken: Bool): FoldedHistory = {
    val oldest_bits = VecInit(get_oldest_bits_from_ghr(ghr, histPtr))
    update(oldest_bits, num, taken)
  }


  // fast path, use pre-read oldest bits
  def update(ob: Vec[Bool], num: Int, taken: Bool): FoldedHistory = {
    // do xors for several bitsets at specified bits
    def bitsets_xor(len: Int, bitsets: Seq[Seq[Tuple2[Int, Bool]]]) = {
      val res = Wire(Vec(len, Bool()))

      val resArr = Array.fill(len)(List[Bool]())
      for (bs <- bitsets) {
        for ((n, b) <- bs) {
          resArr(n) = b :: resArr(n)
        }
      }

      for (i <- 0 until len) {
        if (resArr(i).length > 2) {
          println(f"[warning] update logic of foldest history has two or more levels of xor gates! " +
            f"histlen:${this.len}, compLen:$compLen, at bit $i")
        }
        if (resArr(i).length == 0) {
          println(f"[error] bits $i is not assigned in folded hist update logic! histlen:${this.len}, compLen:$compLen")
        }
        res(i) := resArr(i).foldLeft(false.B)(_^_)
      }
      res.asUInt
    }

    val new_folded_hist = if (need_oldest_bits) {
      val oldest_bits = ob
      require(oldest_bits.length == max_update_num)
      // mask off bits that do not update
      val oldest_bits_masked = oldest_bits.zipWithIndex.map{
        case (ob, i) => ob && (i < num).B
      }
      // if a bit does not wrap around, it should not be xored when it exits
      val oldest_bits_set = (0 until max_update_num).filter(oldest_bit_wrap_around).map(i => (oldest_bit_pos_in_folded(i), oldest_bits_masked(i)))

  
      // only the last bit could be 1, as we have at most one taken branch at a time
      val newest_bits_masked = VecInit((0 until max_update_num).map(i => taken && ((i + 1) == num).B)).asUInt
      // if a bit does not wrap around, newest bits should not be xored onto it either
      val newest_bits_set = (0 until max_update_num).map(i => (compLen-1-i, newest_bits_masked(i)))
  

      val original_bits_masked = VecInit(foldedHist.asBools.zipWithIndex.map{
        case (fb, i) => fb && !(num >= (len-i)).B
      })
      val original_bits_set = (0 until compLen).map(i => (i, original_bits_masked(i)))

      // do xor then shift
      val xored = bitsets_xor(compLen, Seq(original_bits_set, oldest_bits_set, newest_bits_set))
      circular_shift_left(xored, num)
    } else {
      // histLen too short to wrap around
      ((foldedHist << num).asUInt | taken)(compLen-1,0)
    }

    val fh = WireInit(this)
    fh.foldedHist := new_folded_hist
    fh
  }
}

class AheadFoldedHistoryOldestBits(val len: Int, val max_update_num: Int)(implicit p: Parameters) extends XSBundle {
  val bits = Vec(max_update_num*2, Bool())
  // def info = (len, compLen)
  def getRealOb(brNumOH: UInt): Vec[Bool] = {
    val ob = Wire(Vec(max_update_num, Bool()))
    for (i <- 0 until max_update_num) {
      ob(i) := Mux1H(brNumOH, bits.slice(i, i + numBr + 1))
    }
    ob
  }
}

class AllAheadFoldedHistoryOldestBits(val gen: Seq[(Int, Int)])(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val afhob = MixedVec(gen.filter(t => t._1 > t._2).map{_._1}
    .toSet.toList.map(l => new AheadFoldedHistoryOldestBits(l, numBr))) // remove duplicates
  require(gen.toSet.toList.equals(gen))
  def getObWithInfo(info: (Int, Int)): AheadFoldedHistoryOldestBits = {
    val selected = afhob.filter(_.len == info._1)
    require(selected.length == 1)
    selected.head
  }
  def read(ghv: Vec[Bool], ptr: CGHPtr): Unit = {
    val hisLens = afhob.map(_.len)
    val bitsToRead = hisLens.flatMap(l => (0 until numBr*2).map(i => l-i-1)).toSet // remove duplicates
    val bitsWithInfo = bitsToRead.map(pos => (pos, ghv((ptr + (pos + 1).U).value)))
    for (ob <- afhob) {
      for (i <- 0 until numBr*2) {
        val pos = ob.len - i - 1
        val bit_found = bitsWithInfo.filter(_._1 == pos).toList
        require(bit_found.length == 1)
        ob.bits(i) := bit_found.head._2
      }
    }
  }
}

class AllFoldedHistories(val gen: Seq[Tuple2[Int, Int]])(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val hist = MixedVec(gen.map{case (l, cl) => new FoldedHistory(l, cl, numBr)})

  require(gen.toSet.toList.equals(gen))
  def getHistWithInfo(info: Tuple2[Int, Int]): FoldedHistory = {
    val selected = hist.filter(_.info.equals(info))
    require(selected.length == 1)
    selected(0)
  }

  def update(ghv: Vec[Bool], ptr: CGHPtr, shift: Int, taken: Bool): AllFoldedHistories = {
    val res = WireInit(this)
    for (i <- 0 until this.hist.length) {
      res.hist(i) := this.hist(i).update(ghv, ptr, shift, taken)
    }
    res
  }

  def update(afhob: AllAheadFoldedHistoryOldestBits, lastBrNumOH: UInt, shift: Int, taken: Bool): AllFoldedHistories = {
    val res = WireInit(this)
    for (i <- 0 until this.hist.length) {
      val fh = this.hist(i)
      if (fh.need_oldest_bits) {
        val info = fh.info
        val selectedAfhob = afhob.getObWithInfo(info)
        val ob = selectedAfhob.getRealOb(lastBrNumOH)
        res.hist(i) := this.hist(i).update(ob, shift, taken)
      } else {
        val dumb = Wire(Vec(numBr, Bool())) // not needed
        dumb := DontCare
        res.hist(i) := this.hist(i).update(dumb, shift, taken)
      }
    }
    res
  }

}

class TableAddr(val idxBits: Int, val banks: Int)(implicit p: Parameters) extends XSBundle{
  def tagBits = VAddrBits - idxBits - instOffsetBits

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val offset = UInt(instOffsetBits.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx
  def getBank(x: UInt) = if (banks > 1) getIdx(x)(log2Up(banks) - 1, 0) else 0.U
  def getBankIdx(x: UInt) = if (banks > 1) getIdx(x)(idxBits - 1, log2Up(banks)) else getIdx(x)
}

trait BasicPrediction extends HasXSParameter {
  def cfiIndex: ValidUndirectioned[UInt]
  def target(pc: UInt): UInt
  def lastBrPosOH: Vec[Bool]
  def brTaken: Bool
  def shouldShiftVec: Bool
  def fallThruError: Bool
}

class FullBranchPrediction(implicit p: Parameters) extends XSBundle with HasBPUConst with BasicPrediction {
  val br_taken = Bool()

  val slotValid = Bool()

  val targets = UInt(VAddrBits.W)
  val jalrTarget = UInt(VAddrBits.W) // special path for indirect predictors
  val offsets = UInt(log2Ceil(PredictWidth).W)
  val fallThroughAddr = UInt(VAddrBits.W)
  val fallThroughErr = Bool()

  val isJal = Bool()
  val isJalr = Bool()
  val isCall = Bool()
  val isRet = Bool()
  val last_may_be_rvi_call = Bool()
  val isBrSharing = Bool()

  // val call_is_rvc = Bool()
  val hit = Bool()

  def brValid = slotValid && isBrSharing

  def takenOnSlot = slotValid && (isBrSharing && br_taken || !isBrSharing)

  def realSlotTaken: Bool = takenOnSlot && hit
  
  // len numBr
  def realBrTaken: Bool = br_taken && slotValid && isBrSharing && hit

  // the vec indicating if ghr should shift on each branch
  def shouldShiftVec = brValid

  def lastBrPosOH = VecInit(Seq(!hit || !brValid, brValid && hit))

  def brTaken = brValid && br_taken && hit

  def target(pc: UInt): UInt = {
    val targetVec = Seq(targets, fallThroughAddr, pc + (FetchWidth * 4).U)
    val selVecOH = Seq(takenOnSlot && hit, !takenOnSlot && hit, !hit)
    Mux1H(selVecOH, targetVec)
  }

  def fallThruError: Bool = hit && fallThroughErr

  def hitTakenOnJmp = realSlotTaken && !isBrSharing
  def hitTakenOnCall = hitTakenOnJmp && isCall
  def hitTakenOnRet  = hitTakenOnJmp && isRet
  def hitTakenOnJalr = hitTakenOnJmp && isJalr

  def cfiIndex = {
    val cfiIndex = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
    cfiIndex.valid := realSlotTaken
    // when no takens, set cfiIndex to PredictWidth-1
    cfiIndex.bits := offsets | Fill(log2Ceil(PredictWidth), !realSlotTaken)
    cfiIndex
  }

  // def taken = br_taken.reduce(_||_) || slotValid.last // || (isJal || isJalr)
  def taken = br_taken || slotValid// || (isJal || isJalr)

  def fromFtbEntry(entry: FTBEntry, pc: UInt):Unit = {
    slotValid  := entry.valid
    targets    := entry.getTarget(pc)
    jalrTarget := targets
    offsets    := entry.offset
    isJal      := entry.valid && entry.isJal
    isJalr     := entry.valid && entry.isJalr
    isCall     := entry.valid && entry.isCall
    isRet      := entry.valid && entry.isRet
    last_may_be_rvi_call := entry.last_may_be_rvi_call
    isBrSharing := entry.valid && entry.sharing
    
    val startLower        = Cat(0.U(1.W),    pc(instOffsetBits+log2Ceil(PredictWidth)-1, instOffsetBits))
    val endLowerwithCarry = Cat(entry.carry, entry.pftAddr)
    fallThroughErr := startLower >= endLowerwithCarry
    fallThroughAddr := Mux(fallThroughErr, pc + (FetchWidth * 4).U, entry.getFallThrough(pc))
  }

  def display(cond: Bool): Unit = {
  }
}

class SpeculativeInfo(implicit p: Parameters) extends XSBundle
  with HasBPUConst with BPUUtils {
  val foldedHist = new AllFoldedHistories(foldedGHistInfos)
  val afhob = new AllAheadFoldedHistoryOldestBits(foldedGHistInfos)
  val lastBrNumOH = UInt((numBr+1).W)
  val histPtr = new CGHPtr
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasTop = new RASEntry
}


class BranchPredictionBundle(implicit p: Parameters) extends XSBundle
  with HasBPUConst with BPUUtils {

  val pc    = Vec(numDup, UInt(VAddrBits.W))
  val valid = Vec(numDup, Bool())

  val fullPred    = Vec(numDup, new FullBranchPrediction)
  val hasRedirect = Vec(numDup, Bool())
  
  val ftqIdx = new FtqPtr

  def target         = VecInit(fullPred.zip(pc).map {case (fp, p) => fp.target(p)})
  def cfiIndex       = VecInit(fullPred.map(_.cfiIndex))
  def lastBrPosOH    = VecInit(fullPred.map(_.lastBrPosOH))
  def brTaken        = VecInit(fullPred.map(_.brTaken))
  def shouldShiftVec = VecInit(fullPred.map(_.shouldShiftVec))
  def fallThruError  = VecInit(fullPred.map(_.fallThruError))

  def taken = VecInit(cfiIndex.map(_.valid))

}


class BranchPredictionResp(implicit p: Parameters) extends XSBundle with HasBPUConst {
  // val valids = Vec(3, Bool())
  val s1 = new BranchPredictionBundle
  val s2 = new BranchPredictionBundle
  val s3 = new BranchPredictionBundle

  val lastStageMeta = UInt(MaxMetaLength.W)
  val lastStageSpecInfo = new SpeculativeInfo
  val lastStageFtbEntry = new FTBEntry

  def selectedRespForFtq: BranchPredictionBundle ={
    val res =
      PriorityMux(Seq(
        (s3.valid(dupForFtq) && s3.hasRedirect(dupForFtq)) -> s3,
        (s2.valid(dupForFtq) && s2.hasRedirect(dupForFtq)) -> s2,
        s1.valid(dupForFtq) -> s1
      ))
    res
  }
  def selectedRespIdxForFtq: UInt =
    PriorityMux(Seq(
      (s3.valid(dupForFtq) && s3.hasRedirect(dupForFtq)) -> BP_S3,
      (s2.valid(dupForFtq) && s2.hasRedirect(dupForFtq)) -> BP_S2,
      s1.valid(dupForFtq) -> BP_S1
    ))
  def lastStage: BranchPredictionBundle = s3
}

class BpuToFtqBundle(implicit p: Parameters) extends BranchPredictionResp {}

class BranchPredictionUpdate(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val pc = UInt(VAddrBits.W)
  val specInfo = new SpeculativeInfo
  val ftbEntry = new FTBEntry()

  val cfi_idx = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val br_taken = Bool()
  val jmp_taken = Bool()
  val mispred_mask = Vec(numBr+1, Bool())
  val predHit = Bool()
  val falseHit = Bool()
  val new_br_insert_pos = Bool()
  val oldEntry = Bool()
  val meta = UInt(MaxMetaLength.W)
  val fullTarget = UInt(VAddrBits.W)
  val fromStage = UInt(2.W)
  val ghist = UInt(HistoryLength.W)

  def isJal = ftbEntry.valid && ftbEntry.isJal
  def isJalr = ftbEntry.valid && ftbEntry.isJalr
  def isCall = ftbEntry.valid && ftbEntry.isCall
  def isRet = ftbEntry.valid && ftbEntry.isRet

}

class BranchPredictionRedirect(implicit p: Parameters) extends Redirect with HasBPUConst {}
