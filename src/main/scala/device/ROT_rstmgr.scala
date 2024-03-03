package device_rot

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper.{RegField, RegFieldAccessType, RegFieldDesc, RegFieldGroup}


class ROT_rstmgr(implicit p: Parameters) extends LazyModule {

  val node = TLRegisterNode(
    address = Seq(AddressSet(0x3b300000L, 0xff)),
    device = new SimpleDevice("rot_rstmgr", Seq()),
    beatBytes = 8,
    concurrency = 1
  ) 

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
  // lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ctrl = Output(Bool())
    })

    val reset_value = RegInit(0.U(32.W))
    io.ctrl := reset_value(0)

    node.regmap(
      0x00 -> Seq(RegField(32, reset_value, RegFieldDesc("ctrl", "rst ctrl Register"))),
    )
 
  }
}

