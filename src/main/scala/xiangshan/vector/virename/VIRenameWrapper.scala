package xiangshan.vector.virename

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

import xiangshan._
import utils._

import xiangshan.vector._

class VIRenameWrapper(implicit p: Parameters) extends VectorBaseModule {
    val io = IO(new Bundle {
        val hasWalk = Output(Bool())
        val in = new Bundle {
            val vcf = new VICtrlFlow
            val vcsr = new VICsrInfo
            val vsignal = new VICtrlSignals
        }
        val out = new VICtrl
    })

    val rename = new VIRename
    val walk = rename.io.hasWalk
    io.hasWalk := walk
    
    
}
