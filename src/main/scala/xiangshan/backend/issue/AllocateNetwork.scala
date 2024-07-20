/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
/***************************************************************************************
 * Author: Liang Sen
 * E-mail: liangsen20z@ict.ac.cn
 * Date: 2023-03-31
 ****************************************************************************************/

package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{MicroOp, XSModule}
import xs.utils.PickOneLow

class Switch2[T <: Data](gen:T) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(2, gen))
    val out = Output(Vec(2,gen))
    val switch = Input(Bool())
  })
  io.out(0) := Mux(io.switch, io.in(1), io.in(0))
  io.out(1) := Mux(io.switch, io.in(0), io.in(1))
}

object LSFRTap {
  def bits0: Seq[Int] = Seq(0)
  def bits1: Seq[Int] = Seq(0)
  def bits2: Seq[Int] = Seq(0, 1)
  def bits3: Seq[Int] = Seq(1, 2)
  def bits4: Seq[Int] = Seq(2, 3)
  def bits5: Seq[Int] = Seq(2, 4)
  def bits6: Seq[Int] = Seq(4, 5)
  def bits7: Seq[Int] = Seq(5, 6)
  def bits8: Seq[Int] = Seq(3, 4, 5, 7)
  def bits9: Seq[Int] = Seq(4, 8)
  def bits10: Seq[Int] = Seq(6, 9)
  def bits11: Seq[Int] = Seq(8, 10)
  def bits12: Seq[Int] = Seq(0, 3, 5 ,11)
  def bits13: Seq[Int] = Seq(0, 2 ,3, 12)
  def bits14: Seq[Int] = Seq(0, 2 ,4, 13)
  def bits15: Seq[Int] = Seq(13, 14)
  def bits16: Seq[Int] = Seq(3, 12, 14, 15)
  def bits17: Seq[Int] = Seq(13, 16)
  def bits18: Seq[Int] = Seq(10, 17)
  def bits19: Seq[Int] = Seq(0, 1, 5, 18)
  def bits20: Seq[Int] = Seq(16, 19)
  def bits21: Seq[Int] = Seq(18, 20)
  def bits22: Seq[Int] = Seq(20, 21)
  def bits23: Seq[Int] = Seq(17, 22)
  def bits24: Seq[Int] = Seq(16, 21, 22, 23)
  def bits25: Seq[Int] = Seq(21, 24)
  def bits26: Seq[Int] = Seq(0, 1, 5, 25)
  def bits27: Seq[Int] = Seq(0, 1, 4, 26)
  def bits28: Seq[Int] = Seq(24, 27)
  def bits29: Seq[Int] = Seq(26, 28)
  def bits30: Seq[Int] = Seq(0, 3, 5, 29)
  def bits31: Seq[Int] = Seq(27, 30)
  def bits32: Seq[Int] = Seq(0, 1, 21, 31)

  def TapSeq = Seq(
    bits0,  bits1,  bits2,  bits3,
    bits4,  bits5,  bits6,  bits7,
    bits8,  bits9,  bits10, bits11,
    bits12, bits13, bits14, bits15,
    bits16, bits17, bits18, bits19,
    bits20, bits21, bits22, bits23,
    bits24, bits25, bits26, bits27,
    bits28, bits29, bits30, bits31,
  )
}
class LSFR(width:Int, init:Option[Int] = None) extends Module{
  val io = IO(new Bundle{
    val en = Input(Bool())
    val value = Output(UInt(width.W))
  })
  require(width > 0)
  private val tap = LSFRTap.TapSeq(width)
  private val lfsr = RegInit(init.getOrElse(-1).U(width.W)) // random initial value based on simulation seed
  private val xor = tap.map(t => lfsr.asBools.reverse(t)).reduce(_^_)
  when(io.en) {
    lfsr := Mux(lfsr === 0.U, 1.U, Cat(xor, lfsr(width - 1, 1)))
  }
  io.value := lfsr
}

class SwitchNetwork[T <: Data](gen:T, channelNum:Int) extends Module{
  require(channelNum == 4, "Only 4 channles are supported for now!")
  //TODO: Number of switches should be calculated from channels number
  private val switchNum = 6
  val io = IO(new Bundle{
    val in = Input(Vec(channelNum, gen))
    val out = Output(Vec(channelNum, gen))
    val ctrl = Input(UInt(switchNum.W))
  })
  private val switchList = List.tabulate(switchNum){_ => Module(new Switch2(gen))}
  (switchList zip io.ctrl.asBools).foreach({case (s, v) => s.io.switch := v})
  private val switchStage1 = Wire(Vec(channelNum, gen))
  private val switchStage2 = Wire(Vec(channelNum, gen))

  switchList(0).io.in(0) := io.in(0)
  switchList(0).io.in(1) := io.in(1)
  switchList(1).io.in(0) := io.in(2)
  switchList(1).io.in(1) := io.in(3)
  switchStage1(0) := switchList(0).io.out(0)
  switchStage1(1) := switchList(0).io.out(1)
  switchStage1(2) := switchList(1).io.out(0)
  switchStage1(3) := switchList(1).io.out(1)

  switchList(2).io.in(0) := switchStage1(0)
  switchList(2).io.in(1) := switchStage1(3)
  switchList(3).io.in(0) := switchStage1(1)
  switchList(3).io.in(1) := switchStage1(2)
  switchStage2(0) := switchList(2).io.out(0)
  switchStage2(3) := switchList(2).io.out(1)
  switchStage2(1) := switchList(3).io.out(0)
  switchStage2(2) := switchList(3).io.out(1)

  switchList(4).io.in(0) := switchStage2(0)
  switchList(4).io.in(1) := switchStage2(1)
  switchList(5).io.in(0) := switchStage2(2)
  switchList(5).io.in(1) := switchStage2(3)
  io.out(0) := switchList(4).io.out(0)
  io.out(1) := switchList(4).io.out(1)
  io.out(2) := switchList(5).io.out(0)
  io.out(3) := switchList(5).io.out(1)
}

class RandomizationNetwork[T <: Data](gen:T, channelNum:Int)(implicit p:Parameters) extends XSModule{
  val io = IO(new Bundle {
    val enqFire = Input(Bool())
    val in = Input(Vec(channelNum,gen))
    val out = Output(Vec(channelNum,gen))
  })
  private val LSFR = Module(new LSFR(log2Ceil(RobSize + 1),Some(39)))
  LSFR.io.en := io.enqFire
  private val switchNetwork = Module(new SwitchNetwork(gen, channelNum))
  switchNetwork.io.ctrl := LSFR.io.value(5, 0)
  switchNetwork.io.in := io.in
  io.out := switchNetwork.io.out
}

class SqueezeCodeRom(addrWidth:Int, dataWidth:Int) extends Module{
  require(addrWidth == 4 && dataWidth == 6)
  val io = IO(new Bundle{
    val addr = Input(UInt(addrWidth.W))
    val data = Output(UInt(dataWidth.W))
  })
  private val mapping = Seq(
    "b0000".U(addrWidth.W) -> "b000000".U(dataWidth.W),
    "b0001".U(addrWidth.W) -> "b000000".U(dataWidth.W),
    "b0010".U(addrWidth.W) -> "b000001".U(dataWidth.W),
    "b0011".U(addrWidth.W) -> "b000000".U(dataWidth.W),
    "b0100".U(addrWidth.W) -> "b011000".U(dataWidth.W),
    "b0101".U(addrWidth.W) -> "b001000".U(dataWidth.W),
    "b0110".U(addrWidth.W) -> "b010110".U(dataWidth.W),
    "b0111".U(addrWidth.W) -> "b000000".U(dataWidth.W),
    "b1000".U(addrWidth.W) -> "b000100".U(dataWidth.W),
    "b1001".U(addrWidth.W) -> "b001010".U(dataWidth.W),
    "b1010".U(addrWidth.W) -> "b001011".U(dataWidth.W),
    "b1011".U(addrWidth.W) -> "b000010".U(dataWidth.W),
    "b1100".U(addrWidth.W) -> "b011101".U(dataWidth.W),
    "b1101".U(addrWidth.W) -> "b010111".U(dataWidth.W),
    "b1110".U(addrWidth.W) -> "b101001".U(dataWidth.W),
    "b1111".U(addrWidth.W) -> "b000000".U(dataWidth.W),
  )
  private val hitVec = mapping.map(_._1 === io.addr)
  private val res = Mux1H(hitVec, mapping.map(_._2))
  io.data := res
}

class SqueezeNetwork[T <: Data](gen:T, channelNum:Int) extends Module{
  //TODO: Width of control code should be calculated from channels number
  val ctrlCodeWidth = 6
  val io = IO(new Bundle {
    val in = Input(Vec(channelNum, Valid(gen)))
    val out = Output(Vec(channelNum, Valid(gen)))
  })
  private val ctrlCodeRom = Module(new SqueezeCodeRom(channelNum, ctrlCodeWidth))
  private val switchNetwork = Module(new SwitchNetwork(Valid(gen), channelNum))
  ctrlCodeRom.io.addr := Cat(io.in.map(_.valid).reverse)
  switchNetwork.io.ctrl := ctrlCodeRom.io.data
  switchNetwork.io.in := io.in
  io.out := switchNetwork.io.out
}

/** {{{
  * Module Name: AllocateNetwork
  *
  * Function Description:
  * Select empty entry of each reservation station bank
  * and route the input [[MicroOp]] to allocated bank
  * and entry. This module assure target bank of each
  * input port is randomnand ready signals are always
  * continuous in the low order of input port.
  *
  * Parameters:
  *   bankNum:
  *     The number of banks.
  *   entryNumPerBank:
  *     The number of entries in a bank.
  *
  * IO:
  *   entriesValidBitVecList: [Input][Vec]
  *     The list of valid bits in each bank.
  *   enqFromDispatch: [Input][Vec][Decoupled]
  *     Enqueue port from dispatch stage.
  *   enqToRs: [Output][Vec][Valid]
  *     Write signals to each reservation bank.
  *     uop:
  *       The enqueuing [[MicroOp]].
  *     addrOH:
  *       The one hot format entry index of the enqueue data.
  * }}}
  */

class AllocateNetwork(bankNum:Int, entryNumPerBank:Int, name:Option[String] = None)(implicit p: Parameters) extends XSModule{
  private val entryIdxOHWidth = entryNumPerBank
  private val bankIdxWidth = bankNum
  val io = IO(new Bundle {
    val allocVec = Input(Vec(bankNum, Bool()))
    val enqFromDispatch = Vec(bankNum, Flipped(DecoupledIO(new MicroOp)))
    val enqToRs = Vec(bankNum, Valid(new MicroOp))
  })
  override val desiredName:String = name.getOrElse("AllocateNetwork")

  for((disp, idx) <- io.enqFromDispatch.zipWithIndex) {
    disp.ready := PopCount(io.allocVec) > idx.U
  }

  private val randomizer = Module(new RandomizationNetwork(Valid(UInt(bankIdxWidth.W)), bankNum))
  randomizer.io.enqFire := io.enqFromDispatch.map(_.fire).reduce(_|_)
  for((in, idx) <- randomizer.io.in.zipWithIndex){
    in.valid := io.allocVec(idx)
    in.bits := (1 << idx).U(bankIdxWidth.W)
  }

  private val squeezer = Module(new SqueezeNetwork(UInt(bankIdxWidth.W), bankNum))
  squeezer.io.in := randomizer.io.out

  for(idx <- io.enqToRs.indices) {
    val sel = squeezer.io.out.map(_.bits(idx))
    io.enqToRs(idx).valid := Mux1H(sel, io.enqFromDispatch.map(_.fire))
    io.enqToRs(idx).bits := Mux1H(sel, io.enqFromDispatch.map(_.bits))
  }

  //Start of randomization network function points assertions
  assert(randomizer.io.out(0).bits =/= randomizer.io.out(1).bits)
  assert(randomizer.io.out(0).bits =/= randomizer.io.out(2).bits)
  assert(randomizer.io.out(0).bits =/= randomizer.io.out(3).bits)
  assert(randomizer.io.out(1).bits =/= randomizer.io.out(2).bits)
  assert(randomizer.io.out(1).bits =/= randomizer.io.out(3).bits)
  assert(randomizer.io.out(2).bits =/= randomizer.io.out(3).bits)

  private val randomizerInValidBitVec = Cat(randomizer.io.in.map(_.valid).reverse)
  private val randomizerOutValidBitVec = Cat(randomizer.io.out.map(_.valid).reverse)
  private val rov = randomizer.io.out.map(i => Mux(i.valid, i.bits, 0.U))
  private val riv = randomizer.io.in.map(i => Mux(i.valid, i.bits, 0.U))
  assert(PopCount(randomizerInValidBitVec) === PopCount(randomizerOutValidBitVec))
  assert(riv(0) === rov(0) || riv(0) === rov(1) || riv(0) === rov(2) || riv(0) === rov(3))
  assert(riv(1) === rov(0) || riv(1) === rov(1) || riv(1) === rov(2) || riv(1) === rov(3))
  assert(riv(2) === rov(0) || riv(2) === rov(1) || riv(2) === rov(2) || riv(2) === rov(3))
  assert(riv(3) === rov(0) || riv(3) === rov(1) || riv(3) === rov(2) || riv(3) === rov(3))
  //End of randomization network function points assertions

  //Start of squeeze network function points assertions
  private val validVec = squeezer.io.out.map(_.valid)
  private val selVec = squeezer.io.out.map(_.bits)
  for(i <- squeezer.io.out.indices) {
    when(validVec(i)) {
      assert(PopCount(validVec.take(i + 1)) === (i + 1).U)
      assert(Mux1H(selVec(i), io.allocVec))
    }
  }
  //End of squeeze network function points assertions
}
