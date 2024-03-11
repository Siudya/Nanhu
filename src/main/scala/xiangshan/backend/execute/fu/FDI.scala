// See README.md for license details.

package xiangshan.backend.execute.fu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils._
import xiangshan._
import xiangshan.backend.execute.fu.csr.HasCSRConst
import xs.utils._

trait FDIConst {
  val NumFDIMemBounds  = 4  // For load/store
  val NumFullFDIMemBounds  = 16  // For load/store
  val NumFDIJumpBounds = 1   // For jal/jalr
  val NumFullFDIJumpBounds  = 4  // For load/store

  // 8 bytes of granularity
  val FDIGrain         = 8
  val FDIGrainBit      = log2Ceil(FDIGrain)   
}

object FDIOp{
  def read   = "b00".U
  def write  = "b01".U
  def jump   = "b10".U

  def apply() = UInt(2.W)
  def isWrite(op:UInt) = op === write
  def isRead(op:UInt)  = op === read
  def isJump(op:UInt)  = op === jump
}

object FDICheckFault {
    def noFDIFault     = "b00".U
    def UReadDascisFault  = "b01".U
    def UWriteFDIFault = "b10".U
    def UJumpFDIFault  = "b11".U

    def apply() = UInt(2.W)
}

// FDI Config
abstract class FDIConfig extends Bundle

class FDIMemConfig extends FDIConfig {
  val v: Bool = Bool()  // valid
  val u: Bool = Bool()  // unused
  val r: Bool = Bool()  // read
  val w: Bool = Bool()  // write
  
  def valid: Bool = v
  def write: Bool = w
  def read: Bool = r
}

class FDIJumpConfig extends FDIConfig {
  val v = Bool()

  def valid: Bool = v
}

object FDIMemConfig  extends FDIMemConfig
object FDIJumpConfig extends FDIJumpConfig

class FDIControlFlow(implicit p: Parameters) extends XSBundle {
  val under_check = Flipped(ValidIO(new Bundle () {
    val pc = UInt(XLEN.W)
    val target = UInt(XLEN.W)
    val pc_in_trust_zone = Bool() 
  }))
  
  val check_result = Output(new Bundle () {
    val control_flow_legal = Bool()
  })
}

class FDIEntry(implicit p: Parameters) extends XSBundle with FDIConst {

  val cfg = new FDIMemConfig
  val boundHi, boundLo = UInt(XLEN.W)

  // Lowest bits read/write as 0
  def boundRegMask: UInt = (~(FDIGrain - 1).U(XLEN.W)).asUInt

  // Only check bounds, not checking permission
  // bounds are 8-byte aligned
  def boundMatch(addr: UInt): Bool = {
    val addrForComp = addr(VAddrBits - 1, FDIGrainBit)
    (addrForComp >= boundLo(VAddrBits - 1, FDIGrainBit)) && (addrForComp < boundHi(VAddrBits - 1, FDIGrainBit))
  }

  // assign values (bounds parameter are XLEN-length)
  def gen(cfg: FDIConfig, boundLo: UInt, boundHi: UInt): Unit = {
    this.cfg := cfg
    this.boundLo := Cat(boundLo(VAddrBits - 1, FDIGrainBit),0.U(FDIGrainBit.W))
    this.boundHi := Cat(boundHi(VAddrBits - 1, FDIGrainBit),0.U(FDIGrainBit.W))
  }
}

class FDIJumpEntry(implicit p: Parameters) extends XSBundle with FDIConst {

  val cfg = new FDIJumpConfig
  val boundHi, boundLo = UInt(XLEN.W)

  // Lowest bits read/write as 0
  def boundRegMask: UInt = (~(FDIGrain - 1).U(XLEN.W)).asUInt

  // Only check bounds, not checking permission
  // bounds are 8-byte aligned
  def boundMatch(addr: UInt): Bool = {
    val addrForComp = addr(VAddrBits - 1, FDIGrainBit)
    (addrForComp >= boundLo(VAddrBits - 1, FDIGrainBit)) && (addrForComp < boundHi(VAddrBits - 1, FDIGrainBit))
  }

  // assign values (bounds parameter are XLEN-length)
  def gen(cfg: FDIConfig, boundLo: UInt, boundHi: UInt): Unit = {
    this.cfg := cfg
    this.boundLo := Cat(boundLo(VAddrBits - 1, FDIGrainBit),0.U(FDIGrainBit.W))
    this.boundHi := Cat(boundHi(VAddrBits - 1, FDIGrainBit),0.U(FDIGrainBit.W))
  }
}

trait FDIMethod extends FDIConst { this: HasXSParameter =>
  def FDIMemInit(): (Vec[UInt], Vec[UInt]) = {
    val FDIMemCfgPerCSR = XLEN / FDIMemConfig.getWidth //64/4=16
    val cfgs = WireInit(0.U.asTypeOf(Vec(NumFullFDIMemBounds / FDIMemCfgPerCSR, UInt(XLEN.W))))
    val bounds = WireInit(0.U.asTypeOf(Vec(NumFDIMemBounds * 2, UInt(XLEN.W))))
    (cfgs, bounds)
  }

  def FDIJumpInit(): (Vec[UInt], Vec[UInt]) = {
    val FDIJumpCfgPerCSR = 4 
    val cfgs = WireInit(0.U.asTypeOf(Vec(NumFullFDIJumpBounds / FDIJumpCfgPerCSR, UInt(XLEN.W))))
    val bounds = WireInit(0.U.asTypeOf(Vec(NumFDIJumpBounds * 2, UInt(XLEN.W))))
    (cfgs, bounds)
  }
  
  /* FDI Memory Bound Register Mapping Generate */ 
  def FDIGenMemMapping(
    mem_init: () => (Vec[UInt], Vec[UInt]),
    memNum: Int = NumFDIMemBounds,
    memCfgBase: Int, memBoundBase: Int,
    memEntries: Vec[FDIEntry],
  ): Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)] = {
    val FDIMemCfgPerCSR = XLEN / FDIMemConfig.getWidth
    def FDIMemCfgIndex(i: Int) = i / FDIMemCfgPerCSR
    // init_value: (cfgs, bounds)
    val mem_init_value = mem_init()

    // FDIConfigs merged into CSR
    val mem_cfg_merged = RegInit(mem_init_value._1)
    val mem_cfgs = WireInit(mem_cfg_merged).asTypeOf(Vec(NumFullFDIMemBounds, new FDIMemConfig))
    val mem_bounds = RegInit(mem_init_value._2)

    // Wire entries to the registers
    for (i <- memEntries.indices) {
      memEntries(i).gen(mem_cfgs(i), boundLo = mem_bounds(i * 2), boundHi = mem_bounds(i * 2 + 1))
    }

    val mem_cfg_mapping = Map(
      (0 until memNum by FDIMemCfgPerCSR).map(i =>
        MaskedRegMap(addr = memCfgBase + FDIMemCfgIndex(i), reg = mem_cfg_merged(i / FDIMemCfgPerCSR))
      ) : _*
    )

    val mem_bound_mapping = Map(
      (0 until memNum * 2).map(i => MaskedRegMap(
        addr = memBoundBase + i, reg = mem_bounds(i),
        wmask = FDIEntry.boundRegMask, rmask = FDIEntry.boundRegMask
      )) : _*
    )

    mem_cfg_mapping ++ mem_bound_mapping
  }

  /* FDI Jump Bound Register Mapping Generate */
  def FDIGenJumpMapping(
    jump_init: () => (Vec[UInt], Vec[UInt]), 
    jumpNum: Int = NumFDIJumpBounds,
    jumpCfgBase: Int, jumpBoundBase: Int,
    jumpEntries: Vec[FDIJumpEntry]
  ): Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)] = {
    val FDIJumpCfgPerCSR = 4
    def FDIJumpCfgIndex(i: Int) = i / FDIJumpCfgPerCSR
    // init_value: (cfgs, bounds)
    val jump_init_value = jump_init()

    class FDIJumpConfigExt extends FDIConfig{
      val reserve = UInt((16 - FDIJumpConfig.getWidth).W)
      val data = new FDIJumpConfig
    } 

    // FDIConfigs merged into CSR
    val jump_cfg_merged = RegInit(jump_init_value._1)
    val jump_cfgs = WireInit(jump_cfg_merged).asTypeOf(Vec(NumFullFDIJumpBounds, new FDIJumpConfigExt))
    val jump_bounds = RegInit(jump_init_value._2)

    // Wire entries to the registers
    for (i <- jumpEntries.indices) {
      jumpEntries(i).gen(jump_cfgs(i).data, boundLo = jump_bounds(i * 2), boundHi = jump_bounds(i * 2 + 1))
    }

    val jump_cfg_mapping = Map(
      (0 until jumpNum by FDIJumpCfgPerCSR).map(i =>
        MaskedRegMap(addr = jumpCfgBase + FDIJumpCfgIndex(i), reg = jump_cfg_merged(i / FDIJumpCfgPerCSR))
      ) : _*
    )

    val jump_bound_mapping = Map(
      (0 until jumpNum * 2).map(i => MaskedRegMap(
        addr = jumpBoundBase + i, reg = jump_bounds(i),
        wmask = FDIEntry.boundRegMask, rmask = FDIEntry.boundRegMask
      )) : _*
    )

    jump_cfg_mapping ++ jump_bound_mapping
  }


  // Singleton companion object for FDIEntry, with implicit parameters set
  private object FDIEntry extends FDIEntry
}

class FDIMemIO(implicit p: Parameters) extends XSBundle with FDIConst {
  val distribute_csr: DistributedCSRIO = Flipped(new DistributedCSRIO())
  val entries: Vec[FDIEntry] = Output(Vec(NumFDIMemBounds, new FDIEntry))
}

class FDIJumpIO(implicit p: Parameters) extends XSBundle with FDIConst {
  val distribute_csr: DistributedCSRIO = Flipped(new DistributedCSRIO())
  val entries: Vec[FDIJumpEntry] = Output(Vec(NumFDIJumpBounds, new FDIJumpEntry))
  val control_flow = new FDIControlFlow
}

class FDIReqBundle(implicit p: Parameters) extends XSBundle with FDIConst {
  val addr = Output(UInt(VAddrBits.W))
  val inUntrustedZone = Output(Bool())
  val operation = Output(FDIOp())
}

class FDIRespBundle(implicit p: Parameters) extends XSBundle with FDIConst{
  val fdi_fault = Output(FDICheckFault())
}

class FDIMemCheckerIO(implicit p: Parameters) extends XSBundle with FDIConst{
  val resource = Flipped(Output(Vec(NumFDIMemBounds, new FDIEntry)))
  val req = Flipped(Valid(new FDIReqBundle()))
  val resp = new FDIRespBundle()

  //connect for every FDI request
  def connect(addr:UInt, inUntrustedZone:Bool, operation: UInt, entries: Vec[FDIEntry]): Unit = {
    this.req.bits.addr := addr
    this.req.bits.inUntrustedZone := inUntrustedZone
    this.req.bits.operation := operation
    this.resource := entries
  }
}

class FDIJumpCheckerIO(implicit p: Parameters) extends XSBundle with FDIConst{
  val pc   = Input(UInt(VAddrBits.W))
  val contro_flow = Flipped(new FDIControlFlow)
  val req = Flipped(Valid(new FDIReqBundle()))
  val resp = new FDIRespBundle()

  //connect for every FDI request
  def connect(pc: UInt, addr:UInt, inUntrustedZone:Bool, operation: UInt, contro_flow: FDIControlFlow): Unit = {
    this.pc  := pc
    this.req.bits.addr := addr
    this.req.bits.inUntrustedZone := inUntrustedZone
    this.req.bits.operation := operation
    this.contro_flow <> contro_flow
  }
}

// This bundle works for FDICall and Jump Exception simultaneously
// TODO: Need a better name
class FDICallJumpExcpIO(implicit p: Parameters) extends XSBundle with FDIConst {
  val isFDICall = Output(Bool())
  val isJumpExcp = Output(Bool())
  val target = Output(UInt(XLEN.W))

  def connect(isFDICall: Bool, isJumpExcp: Bool, target: UInt): Unit = {
    this.isFDICall := isFDICall
    this.isJumpExcp := isJumpExcp
    this.target := target
  }
}

class MemFDI(implicit p: Parameters) extends XSModule with FDIMethod with HasCSRConst {
  val io: FDIMemIO = IO(new FDIMemIO())

  val w = io.distribute_csr.w

  private val fdi = Wire(Vec(NumFDIMemBounds, new FDIEntry))
  val mapping = FDIGenMemMapping(mem_init = FDIMemInit, memCfgBase = FDILibCfgBase, memBoundBase = FDILibBoundBase, memEntries = fdi)

  val rdata: UInt = Wire(UInt(XLEN.W))
  MaskedRegMap.generate(mapping, w.bits.addr, rdata, w.valid, w.bits.data)


  io.entries := fdi
}

class JumpFDI(implicit p: Parameters) extends XSModule 
  with FDIMethod 
  with FDICheckerMethod
  with HasCSRConst 
{
  val io: FDIJumpIO = IO(new FDIJumpIO())

  val w = io.distribute_csr.w

  private val fdi = Wire(Vec(NumFDIJumpBounds, new FDIJumpEntry))
  val mapping = FDIGenJumpMapping(jump_init = FDIJumpInit, jumpCfgBase = FDIJmpCfgBase, jumpBoundBase = FDIJmpBoundBase, jumpEntries = fdi)

  private val fdi_main_call = RegInit(0.U(XLEN.W))
  private val fdi_return_pc = RegInit(0.U(XLEN.W))
  private val fdi_azone_return_pc = RegInit(0.U(XLEN.W))
  private val fdi_umain_cfg = RegInit(0.U(XLEN.W))
  private val fdi_umain_bound_hi = RegInit(0.U(XLEN.W))
  private val fdi_umain_bound_lo = RegInit(0.U(XLEN.W))

  val control_flow_mapping = Map(
    MaskedRegMap(Fdimaincall, fdi_main_call),
    MaskedRegMap(Fdireturnpc, fdi_return_pc),
    MaskedRegMap(Fdiumaincfg, fdi_umain_cfg, "h2".U(XLEN.W)),
    MaskedRegMap(Fdiumainboundlo, fdi_umain_bound_lo),
    MaskedRegMap(Fdiumainboundhi, fdi_umain_bound_hi)
  )

  val rdata: UInt = Wire(UInt(XLEN.W))
  MaskedRegMap.generate(mapping ++ control_flow_mapping, w.bits.addr, rdata, w.valid, w.bits.data)

  io.entries := fdi

  //FDI jump checker control flow checking
  val (target, pc) = (io.control_flow.under_check.bits.target, io.control_flow.under_check.bits.pc)

  private val mainCfg = Wire(new FDIMainCfg())
  mainCfg.gen(fdi_umain_cfg)
  private val boundLo = fdi_umain_bound_lo
  private val boundHi = fdi_umain_bound_hi

  val isTrustedZone = io.control_flow.under_check.valid && io.control_flow.under_check.bits.pc_in_trust_zone
  val targetInTrustedZone = io.control_flow.under_check.valid && mainCfg.uEnable &&
    fdi_jump_in_bound(addr = target(VAddrBits -1, 0), boundHi = boundHi(VAddrBits -1, 0), boundLo = boundLo(VAddrBits -1, 0))
  
  val targetInActiveZone  = io.control_flow.under_check.valid && !fdi_jump_check(target, fdi)

  val legalJumpTarget = isTrustedZone  || 
                        (!isTrustedZone &&  targetInTrustedZone && (target === fdi_return_pc || target === fdi_main_call)) ||
                        targetInActiveZone

  io.control_flow.check_result.control_flow_legal := legalJumpTarget

}

trait FDICheckerMethod extends FDIConst{
  //def fdi_check(addr:UInt, isUntrustedZone: Bool, op: UInt, FDI: Vec[FDIEntry]): Bool
  def fdi_mem_check(req: Valid[FDIReqBundle], fdi: Vec[FDIEntry]): Bool = {
    val inBoundVec = VecInit(fdi.map(entry => entry.cfg.valid && entry.boundMatch(req.bits.addr)))
    val boundMatchVec = fdi.zipWithIndex.map{case(entry, index)=>
      inBoundVec(index) && ( FDIOp.isRead(req.bits.operation) &&  entry.cfg.r || FDIOp.isWrite(req.bits.operation) && entry.cfg.w )
    }
    !boundMatchVec.reduce(_ || _) && req.bits.inUntrustedZone && req.valid
  }

  def fdi_jump_check(req: Valid[FDIReqBundle], fdi: Vec[FDIJumpEntry]): Bool = {
    val inBoundVec = VecInit(fdi.map(entry => entry.cfg.valid && entry.boundMatch(req.bits.addr)))
    val boundMatchVec = fdi.zipWithIndex.map{case(entry, index)=>
      inBoundVec(index) &&  FDIOp.isJump(req.bits.operation)
    }
    !boundMatchVec.reduce(_ || _) && req.bits.inUntrustedZone && req.valid
  }
  def fdi_jump_check(addr: UInt, fdi: Vec[FDIJumpEntry]): Bool = {
    val inBoundVec = VecInit(fdi.map(entry => entry.cfg.valid && entry.boundMatch(addr)))
    val boundMatchVec = fdi.zipWithIndex.map{case(entry, index)=>
      inBoundVec(index)
    }
    !boundMatchVec.reduce(_ || _)
  }
  def fdi_jump_in_bound(addr: UInt, boundHi:UInt, boundLo:UInt): Bool ={
    //warning VAddrBits may cause bug?
    val addrForComp = addr(addr.getWidth - 1, FDIGrainBit)
    (addrForComp >= boundLo(boundLo.getWidth - 1, FDIGrainBit)) && (addrForComp < boundHi(boundHi.getWidth - 1, FDIGrainBit))
  }
}


class FDIMemChecker(implicit p: Parameters) extends XSModule
  with FDICheckerMethod
  with FDIConst
  with HasCSRConst
{
  val io = IO(new FDIMemCheckerIO)

  val req = io.req
  val fdi_entries = io.resource

  val fdi_mem_fault = RegNext(fdi_mem_check(req, fdi_entries), init = false.B)
  
  io.resp.fdi_fault := FDICheckFault.noFDIFault 

  when(FDIOp.isRead(req.bits.operation) && fdi_mem_fault){
    io.resp.fdi_fault := FDICheckFault.UReadDascisFault
  }.elsewhen(FDIOp.isWrite(req.bits.operation) && fdi_mem_fault){
    io.resp.fdi_fault := FDICheckFault.UWriteFDIFault
  }    
    
}

class FDIJumpChecker(implicit p: Parameters) extends XSModule
  with FDICheckerMethod
  with FDIConst
  with HasCSRConst
{
  val io = IO(new FDIJumpCheckerIO)

  val req = io.req
  val fdi_contro_flow = io.contro_flow

  val fdi_jump_fault = req.valid && !fdi_contro_flow.check_result.control_flow_legal

  fdi_contro_flow.under_check.valid := req.valid
  fdi_contro_flow.under_check.bits.pc   := io.pc
  fdi_contro_flow.under_check.bits.pc_in_trust_zone := !io.req.bits.inUntrustedZone
  fdi_contro_flow.under_check.bits.target := req.bits.addr

  //FDI jump bound checking
  io.resp.fdi_fault := FDICheckFault.noFDIFault 
  when(FDIOp.isJump(req.bits.operation) && fdi_jump_fault){
    io.resp.fdi_fault := FDICheckFault.UJumpFDIFault
  }
}


class FDIMainCfg(implicit p: Parameters) extends XSBundle {
  val uEnable, sEnable = Bool()

  private val UENA = 0x1
  private val SENA = 0x0

  def gen(reg: UInt): Unit = {
    this.uEnable := reg(UENA)
    this.sEnable := reg(SENA)
  }
}

class FDIMainBound(implicit p: Parameters) extends XSBundle with FDIConst {
  val boundHi, boundLo = UInt((VAddrBits - FDIGrainBit).W)

  def getPcTags(startAddr: UInt): Vec[Bool] = {
    val startBlock = startAddr(VAddrBits - 1, FDIGrainBit)
    val startOffset = startAddr(FDIGrainBit - 1, 1) // instructions are 2-byte aligned

    // diff{Lo,Hi}: (VAddrBits, FDIGrainBit) (with a sign bit)
    val diffLo = boundLo -& startBlock
    val diffHi = boundHi -& startBlock

    val fetchBlock = FetchWidth * 4
    val fetchGrain = log2Ceil(fetchBlock) // MinimalConfig: 4; DefConfig: 8
    val numFDIBlocks = fetchBlock / FDIGrain // MinConf: 2; DefConf: 4
    val instPerFDIBlock = FDIGrain / 2 // 4 compressed instructions per FDI block

    // detect edge cases
    val loClose = diffLo(VAddrBits - FDIGrainBit, fetchGrain - FDIGrainBit + 1) === 0.U
    val hiClose = diffHi(VAddrBits - FDIGrainBit, fetchGrain - FDIGrainBit + 1) === 0.U

    // get the low bits (fetchGrain, 0)
    val diffLoLSB = diffLo(fetchGrain - FDIGrainBit, 0)
    val diffHiLSB = diffHi(fetchGrain - FDIGrainBit, 0)

    val maskGen = 0.U((numFDIBlocks + 1).W) // MinConf: 000; DefConf: 00000
    val loBlockMask = (~maskGen << diffLoLSB)(numFDIBlocks, 0).asBools
    val loCloseMask =
      (VecInit(loBlockMask.map(Fill(instPerFDIBlock, _))).asUInt >> startOffset)(FetchWidth * 2 - 1, 0)
    val hiBlockMask = (Cat(maskGen, ~maskGen) << diffHiLSB)(2 * numFDIBlocks + 1, numFDIBlocks + 1).asBools
    val hiCloseMask =
      (VecInit(hiBlockMask.map(Fill(instPerFDIBlock, _))).asUInt >> startOffset)(FetchWidth * 2 - 1, 0)

    val loMask = Mux(
      diffLo(VAddrBits - FDIGrainBit),
      Fill(FetchWidth * 2, 1.U(1.W)), // boundLo < startAddr
      Mux(loClose, loCloseMask, 0.U((FetchWidth * 2).W))
    )
    val hiMask = Mux(
      diffHi(VAddrBits - FDIGrainBit),
      0.U((FetchWidth * 2).W),  // boundHi < startAddr
      Mux(hiClose, hiCloseMask, Fill(FetchWidth * 2, 1.U(1.W)))
    )

    VecInit((~(loMask & hiMask)).asBools) // tags mean untrusted, so revert them
  }

  // assign values (parameters are XLEN-length)
  def gen(boundLo: UInt, boundHi: UInt): Unit = {
    this.boundLo := boundLo(VAddrBits - 1, FDIGrainBit)
    this.boundHi := boundHi(VAddrBits - 1, FDIGrainBit)
  }
}

class FDITaggerIO(implicit p: Parameters) extends XSBundle {
  val distribute_csr: DistributedCSRIO = Flipped(new DistributedCSRIO())
  val privMode: UInt = Input(UInt(2.W))
  val addr: UInt = Input(UInt(VAddrBits.W))
  // TODO: change FetchWidth * 2 to PredictWidth, by accounting for non-C extension
  val notTrusted: Vec[Bool] = Output(Vec(FetchWidth * 2, Bool()))
}

// Tag every instruction as trusted/untrusted in frontend
class FDITagger(implicit p: Parameters) extends XSModule with HasCSRConst {
  val io: FDITaggerIO = IO(new FDITaggerIO())

  private val mainCfgReg = RegInit(UInt(XLEN.W), 0.U)
  private val sMainBoundHi = RegInit(UInt(XLEN.W), 0.U)
  private val sMainBoundLo = RegInit(UInt(XLEN.W), 0.U)
  private val fdi_umain_bound_hi = RegInit(UInt(XLEN.W), 0.U)
  private val fdi_umain_bound_lo = RegInit(UInt(XLEN.W), 0.U)

  private val mainCfg = Wire(new FDIMainCfg())
  mainCfg.gen(mainCfgReg)
  private val mainBound = Wire(new FDIMainBound())
  private val boundLo = Mux(io.privMode === ModeU, fdi_umain_bound_lo, sMainBoundLo)
  private val boundHi = Mux(io.privMode === ModeU, fdi_umain_bound_hi, sMainBoundHi)
  mainBound.gen(boundLo, boundHi)
  private val cmpTags = mainBound.getPcTags(io.addr)
  io.notTrusted := Mux(
    io.privMode === ModeU && mainCfg.uEnable,
    cmpTags,
    VecInit(Seq.fill(FetchWidth * 2)(false.B))
  )

  val w = io.distribute_csr.w
  val mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)] = Map(
    MaskedRegMap(Fdiumaincfg, mainCfgReg, "h2".U(XLEN.W)),
    MaskedRegMap(Fdiumainboundlo, fdi_umain_bound_lo),
    MaskedRegMap(Fdiumainboundhi, fdi_umain_bound_hi)
  )
  val rdata: UInt = Wire(UInt(XLEN.W))
  MaskedRegMap.generate(mapping, w.bits.addr, rdata, w.valid, w.bits.data)
}
