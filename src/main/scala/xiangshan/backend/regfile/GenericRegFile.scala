/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
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
 * Date: 2023-06-19
 ****************************************************************************************/
package xiangshan.backend.regfile

import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import xiangshan.{ExuInput, Redirect, SrcType, XSBundle, XSModule}
import freechips.rocketchip.config.Parameters
import xiangshan.backend.issue.RsIdx

class WritePort(dataWidth:Int, hasMask:Boolean)(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val data = Input(UInt(dataWidth.W))
  val mask = if(hasMask) Some(Input(UInt((dataWidth / 8).W))) else None
  val en = Input(Bool())
}

class ReadPort(dataWidth:Int)(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val data = Output(UInt(dataWidth.W))
}

class GenericRegFile(entriesNum:Int, writeBackNum:Int, bypassNum:Int, readPortNum:Int, dataWidth:Int, moduleName:String, hasZero:Boolean, hasMask:Boolean = false)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val read = Vec(readPortNum, new ReadPort(dataWidth))
    val write = Vec(writeBackNum, new WritePort(dataWidth, hasMask))
    val bypassWrite = Vec(bypassNum, new WritePort(dataWidth, hasMask))
  })
  override val desiredName = moduleName
  println(s"${moduleName} read ports: $readPortNum regular write ports: $writeBackNum bypass write ports $bypassNum")

  if(hasMask) {
    require(!hasZero)
    val bankNum = dataWidth / 8
    val mem = Mem(entriesNum, Vec(bankNum, UInt(8.W)))
    (io.write ++ io.bypassWrite).foreach(w => {
      val writeData = Wire(Vec(bankNum, UInt(8.W)))
      writeData.zipWithIndex.foreach({ case (d, i) => d := w.data(i * 8 + 7, i * 8) })
      when(w.en) {
        mem.write(w.addr, writeData, w.mask.get.asBools)
      }
    })

    io.read.foreach(r => {
      val memReadData = Cat(mem(r.addr).reverse)
      if (bypassNum > 0) {
        val bypassHits = io.bypassWrite.map(w => w.en && w.addr === r.addr)
        val bypassData = Mux1H(bypassHits, io.bypassWrite.map(_.data))
        val bypassValid = bypassHits.reduce(_ | _)
        r.data := Mux(bypassValid, bypassData, memReadData)
      } else {
        r.data := memReadData
      }
    })
  } else {
    val mem = Reg(Vec(entriesNum, UInt(dataWidth.W)))
    val writes = io.write ++ io.bypassWrite
    mem.zipWithIndex.foreach({case(m, i) =>
      val hitVec = writes.map(w => w.en && w.addr === i.U)
      val dataSel = Mux1H(hitVec, writes.map(_.data))
      val wen = Cat(hitVec).orR
      when(wen){
        m := dataSel
      }
    })

    if(hasZero) mem(0) := 0.U

    io.read.foreach(r => {
      val memReadData = Mux1H(UIntToOH(r.addr), mem)
      if (bypassNum > 0) {
        val bypassHits = io.bypassWrite.map(w => w.en && w.addr === r.addr)
        val bypassData = Mux1H(bypassHits, io.bypassWrite.map(_.data))
        val bypassValid = bypassHits.reduce(_ | _)
        r.data := Mux(bypassValid, bypassData, memReadData)
      } else {
        r.data := memReadData
      }
    })
  }
}