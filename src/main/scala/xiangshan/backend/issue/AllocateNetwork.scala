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
/***************************************************************************************
 * Author: Liang Sen
 * E-mail: liangsen20z@ict.ac.cn
 * Date: 2023-03-31
 ****************************************************************************************/

package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.MicroOp
import xs.utils.Assertion.xs_assert
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

class LSFR(width:Int, init:Option[Int] = None) extends Module{
  val io = IO(new Bundle{
    val en = Input(Bool())
    val value = Output(UInt(width.W))
  })
  private val lfsr = RegInit(init.getOrElse(-1).U(width.W)) // random initial value based on simulation seed
  private val xor = lfsr.asBools.reduce(_^_)
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

class RandomizationNetwork[T <: Data](gen:T, channelNum:Int) extends Module{
  val io = IO(new Bundle {
    val enqFire = Input(Bool())
    val in = Input(Vec(channelNum,gen))
    val out = Output(Vec(channelNum,gen))
  })
  private val LSFR = Module(new LSFR(6,Some(39)))
  LSFR.io.en := io.enqFire
  private val switchNetwork = Module(new SwitchNetwork(gen, channelNum))
  switchNetwork.io.ctrl := LSFR.io.value
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
    "b1100".U(addrWidth.W) -> "b001101".U(dataWidth.W),
    "b1101".U(addrWidth.W) -> "b010111".U(dataWidth.W),
    "b1110".U(addrWidth.W) -> "b101001".U(dataWidth.W),
    "b1111".U(addrWidth.W) -> "b000000".U(dataWidth.W),
  )
  private val hitVec = mapping.map(_._1 === io.addr)
  private val res = Mux1H(hitVec, mapping.map(_._2))
  io.data := res
}

class SqueezeNetwork[T <: Valid[K], K <: Data](gen:T, channelNum:Int) extends Module{
  //TODO: Width of control code should be calculated from channels number
  val ctrlCodeWidth = 6
  val io = IO(new Bundle {
    val in = Input(Vec(channelNum, gen))
    val out = Output(Vec(channelNum, gen))
  })
  private val ctrlCode = Wire(UInt(ctrlCodeWidth.W))
  private val ctrlCodeRom = Module(new SqueezeCodeRom(channelNum, ctrlCodeWidth))
  private val switchNetwork = Module(new SwitchNetwork(gen, channelNum))
  ctrlCodeRom.io.addr := Cat(io.in.map(_.valid).reverse)
  ctrlCode := ctrlCodeRom.io.data
  switchNetwork.io.ctrl := ctrlCode
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

class AllocateNetwork(bankNum:Int, entryNumPerBank:Int, name:Option[String] = None)(implicit p: Parameters) extends Module{
  private val entryIdxOHWidth = entryNumPerBank
  private val bankIdxWidth = log2Ceil(bankNum)
  val io = IO(new Bundle {
    val entriesValidBitVecList = Input(Vec(bankNum, UInt(entryNumPerBank.W)))
    val enqFromDispatch = Vec(bankNum, Flipped(DecoupledIO(new MicroOp)))
    val enqToRs = Vec(bankNum, Valid(new Bundle{
      val uop = new MicroOp
      val addrOH = UInt(entryIdxOHWidth.W)
    }))
  })
  override val desiredName:String = name.getOrElse("AllocateNetwork")

  private val entryIdxList = io.entriesValidBitVecList.map(PickOneLow.apply)
  private val bankIdxList = entryIdxList.map(_.valid).zipWithIndex.map({ case(bankValid, bankIdx) =>
    val res = Wire(Valid(UInt(bankIdxWidth.W)))
    res.bits := bankIdx.U(bankIdxWidth.W)
    res.valid := bankValid
    res
  })

  private val randomizer = Module(new RandomizationNetwork(Valid(UInt(bankIdxWidth.W)), bankNum))
  randomizer.io.enqFire := io.enqFromDispatch.map(_.fire).reduce(_|_)
  for((s,m) <- (randomizer.io.in zip bankIdxList)){s := m}

  private val squeezer = Module(new SqueezeNetwork[Valid[UInt], UInt](Valid(UInt(bankIdxWidth.W)), bankNum))
  squeezer.io.in := randomizer.io.out

  for((port, sig) <- io.enqToRs.map(_.bits.addrOH) zip entryIdxList.map(_.bits)) {port := sig}

  private val enqPortSelOHList = Seq.tabulate(bankNum)(idx => squeezer.io.out.map(_.bits === idx.U))
  private val portDataList = enqPortSelOHList.map(Mux1H(_,io.enqFromDispatch))
  for((port, data) <- io.enqToRs.zip(portDataList)) {
    port.bits.uop := data.bits
    port.valid := data.fire
  }

  for((port, sig) <- io.enqFromDispatch.zip(squeezer.io.out)){
    port.ready := sig.valid
  }
  //Start of randomization network function points assertions
  xs_assert(randomizer.io.out(0).bits =/= randomizer.io.out(1).bits)
  xs_assert(randomizer.io.out(0).bits =/= randomizer.io.out(2).bits)
  xs_assert(randomizer.io.out(0).bits =/= randomizer.io.out(3).bits)
  xs_assert(randomizer.io.out(1).bits =/= randomizer.io.out(2).bits)
  xs_assert(randomizer.io.out(1).bits =/= randomizer.io.out(3).bits)
  xs_assert(randomizer.io.out(2).bits =/= randomizer.io.out(3).bits)

  private val randomizerInValidBitVec = Cat(randomizer.io.in.map(_.valid).reverse)
  private val randomizerOutValidBitVec = Cat(randomizer.io.out.map(_.valid).reverse)
  private val rov = randomizer.io.out.map(i => Mux(i.valid, i.bits, 0.U))
  private val riv = randomizer.io.in.map(i => Mux(i.valid, i.bits, 0.U))
  xs_assert(PopCount(randomizerInValidBitVec) === PopCount(randomizerOutValidBitVec))
  xs_assert(riv(0) === rov(0) || riv(0) === rov(1) || riv(0) === rov(2) || riv(0) === rov(3))
  xs_assert(riv(1) === rov(0) || riv(1) === rov(1) || riv(1) === rov(2) || riv(1) === rov(3))
  xs_assert(riv(2) === rov(0) || riv(2) === rov(1) || riv(2) === rov(2) || riv(2) === rov(3))
  xs_assert(riv(3) === rov(0) || riv(3) === rov(1) || riv(3) === rov(2) || riv(3) === rov(3))
  //End of randomization network function points assertions

  //Start of squeeze network function points assertions
  xs_assert(squeezer.io.out(0).bits =/= squeezer.io.out(1).bits)
  xs_assert(squeezer.io.out(0).bits =/= squeezer.io.out(2).bits)
  xs_assert(squeezer.io.out(0).bits =/= squeezer.io.out(3).bits)
  xs_assert(squeezer.io.out(1).bits =/= squeezer.io.out(2).bits)
  xs_assert(squeezer.io.out(1).bits =/= squeezer.io.out(3).bits)
  xs_assert(squeezer.io.out(2).bits =/= squeezer.io.out(3).bits)

  private val squeezerInValidBitVec = Cat(squeezer.io.in.map(_.valid).reverse)
  private val squeezerOutValidBitVec = Cat(squeezer.io.out.map(_.valid).reverse)
  xs_assert(Mux(PopCount(squeezerOutValidBitVec) === 1.U, squeezerOutValidBitVec === "b0001".U, true.B))
  xs_assert(Mux(PopCount(squeezerOutValidBitVec) === 2.U, squeezerOutValidBitVec === "b0011".U, true.B))
  xs_assert(Mux(PopCount(squeezerOutValidBitVec) === 3.U, squeezerOutValidBitVec === "b0111".U, true.B))
  xs_assert(Mux(PopCount(squeezerOutValidBitVec) === 4.U, squeezerOutValidBitVec === "b1111".U, true.B))


  private val so = squeezer.io.out.map(_.bits)
  private val si = squeezer.io.in.map(_.bits)
  xs_assert(Mux(squeezerInValidBitVec === "b0001".U, so(0) === si(0),                                       true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b0010".U, so(0) === si(1),                                       true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b0011".U, so(0) === si(0) && so(1) === si(1),                    true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b0100".U, so(0) === si(2),                                       true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b0101".U, so(0) === si(0) && so(1) === si(2),                    true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b0110".U, so(0) === si(1) && so(1) === si(2),                    true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b0111".U, so(0) === si(0) && so(1) === si(1) && so(2) === si(2), true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b1000".U, so(0) === si(3),                                       true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b1001".U, so(0) === si(0) && so(1) === si(3),                    true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b1010".U, so(0) === si(1) && so(1) === si(3),                    true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b1011".U, so(0) === si(0) && so(1) === si(1) && so(2) === si(3), true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b1100".U, so(0) === si(2) && so(1) === si(3),                    true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b1101".U, so(0) === si(0) && so(1) === si(2) && so(2) === si(3), true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b1110".U, so(0) === si(1) && so(1) === si(2) && so(2) === si(3), true.B))
  xs_assert(Mux(squeezerInValidBitVec === "b1111".U, so(0) === si(0) && so(1) === si(1) && so(2) === si(2) && so(3) === si(3), true.B))
  //End of squeeze network function points assertions

  //Start of enqueue routing function points assertions
  xs_assert(Mux(io.enqToRs(0).valid && squeezer.io.out(0).bits === 0.U, io.enqFromDispatch(0).bits.robIdx === io.enqToRs(0).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(0).valid && squeezer.io.out(1).bits === 0.U, io.enqFromDispatch(1).bits.robIdx === io.enqToRs(0).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(0).valid && squeezer.io.out(2).bits === 0.U, io.enqFromDispatch(2).bits.robIdx === io.enqToRs(0).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(0).valid && squeezer.io.out(3).bits === 0.U, io.enqFromDispatch(3).bits.robIdx === io.enqToRs(0).bits.uop.robIdx, true.B))

  xs_assert(Mux(io.enqToRs(1).valid && squeezer.io.out(0).bits === 1.U, io.enqFromDispatch(0).bits.robIdx === io.enqToRs(1).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(1).valid && squeezer.io.out(1).bits === 1.U, io.enqFromDispatch(1).bits.robIdx === io.enqToRs(1).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(1).valid && squeezer.io.out(2).bits === 1.U, io.enqFromDispatch(2).bits.robIdx === io.enqToRs(1).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(1).valid && squeezer.io.out(3).bits === 1.U, io.enqFromDispatch(3).bits.robIdx === io.enqToRs(1).bits.uop.robIdx, true.B))

  xs_assert(Mux(io.enqToRs(2).valid && squeezer.io.out(0).bits === 2.U, io.enqFromDispatch(0).bits.robIdx === io.enqToRs(2).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(2).valid && squeezer.io.out(1).bits === 2.U, io.enqFromDispatch(1).bits.robIdx === io.enqToRs(2).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(2).valid && squeezer.io.out(2).bits === 2.U, io.enqFromDispatch(2).bits.robIdx === io.enqToRs(2).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(2).valid && squeezer.io.out(3).bits === 2.U, io.enqFromDispatch(3).bits.robIdx === io.enqToRs(2).bits.uop.robIdx, true.B))

  xs_assert(Mux(io.enqToRs(3).valid && squeezer.io.out(0).bits === 3.U, io.enqFromDispatch(0).bits.robIdx === io.enqToRs(3).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(3).valid && squeezer.io.out(1).bits === 3.U, io.enqFromDispatch(1).bits.robIdx === io.enqToRs(3).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(3).valid && squeezer.io.out(2).bits === 3.U, io.enqFromDispatch(2).bits.robIdx === io.enqToRs(3).bits.uop.robIdx, true.B))
  xs_assert(Mux(io.enqToRs(3).valid && squeezer.io.out(3).bits === 3.U, io.enqFromDispatch(3).bits.robIdx === io.enqToRs(3).bits.uop.robIdx, true.B))
  //End of enqueue routing function points assertions
}
