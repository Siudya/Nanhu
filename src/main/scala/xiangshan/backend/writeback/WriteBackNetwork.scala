package xiangshan.backend.writeback

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.exu.ExuType
import freechips.rocketchip.diplomacy._
import xiangshan.{HasXSParameter, Redirect}
import xiangshan.frontend.Ftq_RF_Components
class WriteBackNetwork(implicit p:Parameters) extends LazyModule{
  val node = new WriteBackNetworkNode

  lazy val module = new LazyModuleImp(this) with HasXSParameter {
    private val wbSources = node.in.map(i => (i._1, i._2._1))
    private val wbSourcesMap = node.in.map(elm => elm._2._1 -> (elm._1, elm._2._1)).toMap
    private val wbSink = node.out

    println("\nWriteback Network Info:")
    println(s"Writeback Num: ${wbSources.length}")
    val io = IO(new Bundle{
      val pcReadAddr = Output(Vec(2, UInt(log2Ceil(FtqSize).W)))
      val pcReadData = Input(Vec(2, new Ftq_RF_Components))
      val redirectOut = Output(Valid(new Redirect))
    })
    private val jmpNum = wbSources.count(_._2.exuType == ExuType.jmp)
    private val aluNum = wbSources.count(_._2.exuType == ExuType.alu)
    private val lduNum = wbSources.count(w => w._2.exuType == ExuType.ldu || w._2.exuType == ExuType.sta)
    private val redirectGen = Module(new RedirectGen(jmpNum, aluNum, lduNum))
    io.pcReadAddr := redirectGen.io.pcReadAddr
    redirectGen.io.pcReadData := io.pcReadData

    private var jmpRedirectIdx = 0
    private var aluRedirectIdx = 0
    private var memRedirectIdx = 0
    wbSources.filter(_._2.hasRedirectOut).foreach(source => {
      if(source._2.exuType == ExuType.jmp){
        redirectGen.io.jmpWbIn(jmpRedirectIdx) := source._1
        jmpRedirectIdx = jmpRedirectIdx + 1
      } else if(source._2.exuType == ExuType.alu){
        redirectGen.io.aluWbIn(aluRedirectIdx) := source._1
        aluRedirectIdx = aluRedirectIdx + 1
      } else if (source._2.exuType == ExuType.sta || source._2.exuType == ExuType.ldu) {
        redirectGen.io.memWbIn(memRedirectIdx) := source._1
        memRedirectIdx = memRedirectIdx + 1
      } else {
        require(false, "Unexpected redirect out exu!")
      }
    })
    private val localRedirectReg = Pipe(redirectGen.io.redirectOut)
    redirectGen.io.redirectIn := localRedirectReg
    io.redirectOut := redirectGen.io.redirectOut

    for(s <- wbSink){
      val sinkParam = s._2._2
      val source = sinkParam.map(elm => wbSourcesMap(elm))
      val sink = s._1
      sink.zip(source).foreach({case(dst, (src,cfg)) =>
        val realSrc = WireInit(src)
        if(s._2._1.needWriteback && cfg.speculativeWakeup){
          val realValid = src.valid && !src.bits.uop.robIdx.needFlush(localRedirectReg)
          realSrc.valid := RegNext(realValid, false.B)
          realSrc.bits.uop := RegEnable(src.bits.uop, realValid)
        }
        if(s._2._1.isRob){
          dst.valid := RegNext(realSrc.valid, false.B)
          dst.bits := RegEnable(realSrc.bits, realSrc.valid)
        } else {
          dst := realSrc
        }
      })
    }
  }
}
