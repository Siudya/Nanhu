package xiangshan.vector.virename

import top._
import chisel3.emitVerilog
import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import xiangshan.XSCoreParamsKey
import chipsalliance.rocketchip.config.Config
import xiangshan.vector._

object generator extends App {
    override def main(args: Array[String]) {
        implicit val p = new Config((_, _, _) => {
            case VectorParametersKey => VectorParameters(128, 4, 4, 64)
        })
        //(new ChiselStage).emitVerilog(new VIFreeList(), Array("-td", "debug_code/"))
        //(new ChiselStage).emitVerilog(new VIRenameTable(), Array("-td", "debug_code/"))
        (new ChiselStage).emitVerilog(new VIRename(), Array("-td", "debug_code/"))
        (new ChiselStage).emitVerilog(new VIRobIdxQueue(16), Array("-td", "debug_code/"))
        (new ChiselStage).emitVerilog(new VIRollBackList, Array("-td", "debug_code/"))
    }
}
