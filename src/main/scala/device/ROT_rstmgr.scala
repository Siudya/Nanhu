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
      val key = Output(UInt(256.W))
      val enable = Output(Bool())
    })

    val reset_value = RegInit(0.U(32.W))
    val key_value = RegInit(0.U(256.W))
    val enable_value = RegInit(false.B)

    io.ctrl := reset_value(0)
    io.key := key_value
    io.enable := enable_value

    node.regmap(
      0x00 -> Seq(RegField(32, reset_value, RegFieldDesc("ctrl", "rst ctrl Register"))),
      // key register spans four 64-bit addresses
      0x08 -> Seq(RegField(64, key_value(63, 0), RegFieldDesc("key_part1", "Key Register Part 1"))),
      0x10 -> Seq(RegField(64, key_value(127, 64), RegFieldDesc("key_part2", "Key Register Part 2"))),
      0x18 -> Seq(RegField(64, key_value(191, 128), RegFieldDesc("key_part3", "Key Register Part 3"))),
      0x20 -> Seq(RegField(64, key_value(255, 192), RegFieldDesc("key_part4", "Key Register Part 4"))),
      0x28 -> Seq(RegField(1, enable_value, RegFieldDesc("enable", "Enable Register")))
    )
 
  }
}

