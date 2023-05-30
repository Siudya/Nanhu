package xiangshan.backend.regfile

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import chipsalliance.rocketchip.config.Parameters
import chisel3.experimental.prefix
import xiangshan.{ExuInput, FuType, HasXSParameter, MicroOp, Redirect, SrcType}
import xiangshan.frontend.Ftq_RF_Components
import xiangshan.backend.execute.fu.fpu.FMAMidResult
import xiangshan.backend.issue.RsIdx
import xiangshan.backend.writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}

class RegFileTop(implicit p:Parameters) extends LazyModule with HasXSParameter{
  val issueNode = new RegFileNode
  val writebackNode = new WriteBackSinkNode(WriteBackSinkParam("RegFile Top", WriteBackSinkType.regFile))

  lazy val module = new LazyModuleImp(this) {
    val pcReadNum:Int = issueNode.out.count(_._2._2.hasJmp) * 2 + issueNode.out.count(_._2._2.hasLoad)
    println("\nRegfile Configuration:")
    println(s"PC read num: $pcReadNum \n")
    println("Regfile Writeback Info:")

    val io = IO(new Bundle{
      val pcReadAddr = Output(Vec(pcReadNum, UInt(log2Ceil(FtqSize).W)))
      val pcReadData = Input(Vec(pcReadNum, new Ftq_RF_Components))
    })
    require(issueNode.in.count(_._2._1.isIntRs) <= 1)
    require(issueNode.in.count(_._2._1.isMemRs) <= 1)
    require(issueNode.in.count(_._2._1.isFpRs) <= 1)
    require(writebackNode.in.length == 1)
    require(issueNode.out.count(_._2._2.hasJmp) == 1)

    private val wb = writebackNode.in.flatMap(i => i._1.zip(i._2._1))
    wb.zipWithIndex.foreach({ case ((_, cfg), idx) =>
      println(s"port $idx ${cfg.name} #${cfg.id} write Int: ${cfg.writeIntRf} write Fp: ${cfg.writeFpRf} bypass Int: ${cfg.bypassIntRegfile} bypass Fp: ${cfg.bypassFpRegfile}")
    })
    println("")

    private val fromRs = issueNode.in.flatMap(i => i._1.zip(i._2._2).map(e => (e._1, e._2, i._2._1)))
    private val toExuMap = issueNode.out.map(i => i._2._2 -> (i._1, i._2._2, i._2._1)).toMap

    private val needIntSrc = issueNode.out.filter(i => i._2._2.readIntegerRegfile).map(i => (i._1, i._2._2, i._2._1))
    private val needFpSrc = issueNode.out.filter(i => i._2._2.readFloatingRegfile).map(i => (i._1, i._2._2, i._2._1))

    private val writeIntRfBypass = wb.filter(i => i._2.bypassIntRegfile)
    private val writeIntRf = wb.filter(i => !i._2.bypassIntRegfile && i._2.writeIntRf)
    private val writeFpRfBypass = wb.filter(i => i._2.bypassFpRegfile)
    private val writeFpRf = wb.filter(i => !i._2.bypassFpRegfile && i._2.writeFpRf)

    private val intRf = Module(new GenericRegFile(NRPhyRegs, writeIntRf.length, writeIntRfBypass.length, needIntSrc.map(_._2.intSrcNum).sum, XLEN, "IntegerRegFile"))
    private val fpRf = Module(new GenericRegFile(NRPhyRegs, writeFpRf.length, writeFpRfBypass.length, needFpSrc.map(_._2.fpSrcNum).sum, XLEN, "FloatingRegFile"))

    private val intWriteBackSinks = intRf.io.write ++ intRf.io.bypassWrite
    private val intWriteBackSources = writeIntRf ++ writeIntRfBypass
    intWriteBackSinks.zip(intWriteBackSources.map(_._1)).foreach({case(sink, source) =>
      sink.en := source.valid && source.bits.uop.ctrl.rfWen
      sink.addr := source.bits.uop.pdest
      sink.data := source.bits.data
    })

    private val fpWriteBackSinks = fpRf.io.write ++ fpRf.io.bypassWrite
    private val fpWriteBackSources = writeFpRf ++ writeFpRfBypass
    fpWriteBackSinks.zip(fpWriteBackSources.map(_._1)).foreach({ case (sink, source) =>
      sink.en := source.valid && source.bits.uop.ctrl.fpWen
      sink.addr := source.bits.uop.pdest
      sink.data := source.bits.data
    })

    private var intRfReadIdx = 0
    private var fpRfReadIdx = 0
    private var pcReadPortIdx = 0
    for(in <- fromRs){
      val out = toExuMap(in._2)
      val rsParam = in._3
      val exuComplexParam = in._2
      val bi = in._1
      val bo = out._1
      prefix(s"${exuComplexParam.name}_${exuComplexParam.id}") {
        val exuInBundle = WireInit(bi.issue.bits)
        exuInBundle.src := DontCare

        if (exuComplexParam.isIntType) {
          val issueBundle = WireInit(bi.issue.bits)
          val srcNum = exuComplexParam.intSrcNum
          for((d, addr) <- issueBundle.src.zip(bi.issue.bits.uop.psrc).take(srcNum)){
            intRf.io.read(intRfReadIdx).addr := addr
            d := Mux(addr === 0.U, 0.U, intRf.io.read(intRfReadIdx).data)
            intRfReadIdx = intRfReadIdx + 1
          }
          if(exuComplexParam.hasJmp){
            io.pcReadAddr(pcReadPortIdx) := bi.issue.bits.uop.cf.ftqPtr.value
            io.pcReadAddr(pcReadPortIdx + 1) := (bi.issue.bits.uop.cf.ftqPtr + 1.U).value
            val instrPc = io.pcReadData(pcReadPortIdx).getPc(bi.issue.bits.uop.cf.ftqOffset)
            val jalrTarget = io.pcReadData(pcReadPortIdx + 1).startAddr
            pcReadPortIdx = pcReadPortIdx + 2
            exuInBundle := ImmExtractor(exuComplexParam, issueBundle, Some(instrPc), Some(jalrTarget))
          } else {
            exuInBundle := ImmExtractor(exuComplexParam, issueBundle)
          }
        } else if(exuComplexParam.isFpType){
          val srcNum = exuComplexParam.fpSrcNum
          for ((d, addr) <- exuInBundle.src.zip(bi.issue.bits.uop.psrc).take(srcNum)) {
            fpRf.io.read(fpRfReadIdx).addr := addr
            d := fpRf.io.read(fpRfReadIdx).data
            fpRfReadIdx = fpRfReadIdx + 1
          }
        } else if(exuComplexParam.isSta) {
          intRf.io.read(intRfReadIdx).addr := bi.issue.bits.uop.psrc(0)
          exuInBundle.src(0) := intRf.io.read(intRfReadIdx).data
          intRfReadIdx = intRfReadIdx + 1
        } else if(exuComplexParam.isLdu) {
          val issueBundle = WireInit(bi.issue.bits)
          io.pcReadAddr(pcReadPortIdx) := bi.issue.bits.uop.cf.ftqPtr.value
          intRf.io.read(intRfReadIdx).addr := bi.issue.bits.uop.psrc(0)
          issueBundle.src(0) := intRf.io.read(intRfReadIdx).data
          issueBundle.uop.cf.pc := io.pcReadData(pcReadPortIdx).getPc(bi.issue.bits.uop.cf.ftqOffset)
          exuInBundle := ImmExtractor(exuComplexParam, issueBundle)
          intRfReadIdx = intRfReadIdx + 1
          pcReadPortIdx = pcReadPortIdx + 1
        } else if(exuComplexParam.isStd) {
          intRf.io.read(intRfReadIdx).addr := bi.issue.bits.uop.psrc(1)
          fpRf.io.read(fpRfReadIdx).addr := bi.issue.bits.uop.psrc(1)
          val intData = intRf.io.read(intRfReadIdx).data
          val fpData = fpRf.io.read(fpRfReadIdx).data
          val srcIsInt = bi.issue.bits.uop.ctrl.srcType(1) === SrcType.reg
          exuInBundle.src(0) := Mux(srcIsInt, intData, fpData)
          intRfReadIdx = intRfReadIdx + 1
          fpRfReadIdx = fpRfReadIdx + 1
        } else {
          exuInBundle := DontCare
          require(false, "Unknown Exu Complex Type")
        }

        val issueValidReg = RegInit(false.B)
        val issueExuInReg = Reg(new ExuInput)
        val rsIdxReg = Reg(new RsIdx)

        val allowPipe = !issueValidReg || bo.issue.fire
        bo.issue.valid := issueValidReg
        bo.issue.bits := issueExuInReg
        bo.rsIdx := rsIdxReg
        when(allowPipe) {
          issueValidReg := bi.issue.valid
        }
        when(bi.issue.fire) {
          issueExuInReg := exuInBundle
          rsIdxReg := bi.rsIdx
        }

        bi.issue.ready := allowPipe
      }
    }
  }
}
