package xiangshan.vector.vbackend.vregfile
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{ExuOutput, XSBundle, XSModule}
import xs.utils.ZeroExt

class MoveReq(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  private val addrBits = coreParams.vectorParameters.vPhyRegIdxWidth
  val srcAddr = UInt(log2Up(addrBits).W)
  val dstAddr = UInt(log2Up(addrBits).W)
  val agnostic = Bool()
  val enable = Bool()
  val sew = UInt(2.W)
  val uopIdx = UInt(7.W)
}

class VrfReadPort(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  val addr = Input(UInt(log2Up(VRFSize).W))
  val data = Output(UInt(VLEN.W))
}

class VRegfile(wbWkpNum:Int, wbNoWkpNum:Int, readPortNum:Int)(implicit p: Parameters) extends XSModule {
  private val size = coreParams.vectorParameters.vPhyRegsNum
  private val addrBits = coreParams.vectorParameters.vPhyRegIdxWidth
  private val dataWidth = VLEN
  private val maskWidth = VLEN / 8
  val io = IO(new Bundle{
    val wbWakeup = Input(Vec(wbWkpNum, Valid(new ExuOutput)))
    val wbNoWakeup = Input(Vec(wbNoWkpNum, Valid(new ExuOutput)))
    val wakeups = Output(Vec(wbWkpNum, Valid(new ExuOutput)))
    val moveOldValReqs = Input(Vec(loadUnitNum, Valid(new MoveReq)))
    val readPorts = Vec(readPortNum, new VrfReadPort)

    val debug = if(env.EnableDifftest || env.AlwaysBasicDiff) {Some(Vec(32, new Bundle {
      val addr = Input(UInt(PhyRegIdxWidth.W))
      val data = Output(UInt(VLEN.W))
    }))} else {
      None
    }
  })

  private val mrf = Mem(size, Vec(maskWidth, Bool()))
  private val vrf = Mem(size, Vec(maskWidth, UInt(8.W)))
  private val fullMaskVec = VecInit(Seq.fill(maskWidth)(true.B))

  // read vector register file
  for (r <- io.readPorts) {
    r.data := vrf(r.addr).asUInt
  }

  //difftest read
  if(io.debug.isDefined){
    io.debug.get.foreach(r => {
      r.data := vrf(r.addr).asUInt
    })
  }

  // write vector register file
  for (i <- 0 until wbWkpNum) {
    val addr = io.wbWakeup(i).bits.uop.pdest(addrBits - 1 ,0)
    val data = io.wbWakeup(i).bits.data.asTypeOf(Vec(dataWidth / 8, UInt(8.W)))
    val wbMask = io.wbWakeup(i).bits.writeDataMask.asBools
    val wkpMask = io.wbWakeup(i).bits.wakeupMask.asBools
    when (io.wbWakeup(i).valid) {
      vrf.write(addr, data, wbMask)
      mrf.write(addr, fullMaskVec, wkpMask)
    }
    // wakeup
    val wbValidReg = RegNext(io.wbWakeup(i).valid, false.B)
    val wbBitsReg = RegEnable(io.wakeups(i).bits, io.wbWakeup(i).valid)
    val wbRedirectValidReg = RegNext(io.wakeups(i).bits.redirectValid, false.B)
    val wbRedirectBitsReg = RegEnable(io.wakeups(i).bits.redirect, false.B)
    val maskRead = mrf.read(wbBitsReg.uop.pdest)
    io.wakeups(i).valid := wbValidReg && maskRead.reduce(_&_)
    io.wakeups(i).bits := wbBitsReg
    io.wakeups(i).bits.redirectValid := wbRedirectValidReg
    io.wakeups(i).bits.redirect := wbRedirectBitsReg
  }
  // not wakeup
  for (i <- 0 until wbNoWkpNum) {
    val addr = io.wbNoWakeup(i).bits.uop.pdest
    val data = io.wbNoWakeup(i).bits.data.asTypeOf(Vec(dataWidth / 8, UInt(8.W)))
    val dataMask = ((1 << maskWidth) - 1).U(maskWidth.W).asBools
    when(io.wbNoWakeup(i).valid) {
      vrf.write(addr, data, dataMask)
    }
  }
  // Move request
  for (i <- 0 until loadUnitNum) {
    val mReq = io.moveOldValReqs(i)
    when (mReq.valid) {
      val dst = io.moveOldValReqs(i).bits.dstAddr
      val srcData = WireInit(vrf.read(mReq.bits.srcAddr))
      val elmIdx = MuxCase(0.U, Seq(
        (mReq.bits.sew === 0.U) -> ZeroExt(mReq.bits.uopIdx(log2Ceil(VLEN / 8) - 1, 0), log2Ceil(VLEN / 8)),
        (mReq.bits.sew === 1.U) -> ZeroExt(mReq.bits.uopIdx(log2Ceil(VLEN / 16) - 1, 0), log2Ceil(VLEN / 8)),
        (mReq.bits.sew === 2.U) -> ZeroExt(mReq.bits.uopIdx(log2Ceil(VLEN / 32) - 1, 0), log2Ceil(VLEN / 8)),
        (mReq.bits.sew === 3.U) -> ZeroExt(mReq.bits.uopIdx(log2Ceil(VLEN / 64) - 1, 0), log2Ceil(VLEN / 8)),
      ))
      val wm = MuxCase(0.U, Seq(
        (mReq.bits.sew === 0.U) -> (0x1.U << elmIdx)(maskWidth - 1, 0),
        (mReq.bits.sew === 1.U) -> (0x3.U << elmIdx)(maskWidth - 1, 0),
        (mReq.bits.sew === 2.U) -> (0xf.U << elmIdx)(maskWidth - 1, 0),
        (mReq.bits.sew === 3.U) -> (0xff.U << elmIdx)(maskWidth - 1, 0),
      ))
      when(mReq.bits.agnostic && !mReq.bits.enable){
        srcData.foreach(_ := 0xffff.U)
      }
      vrf.write(dst, srcData, wm.asBools)
    }
  }
}