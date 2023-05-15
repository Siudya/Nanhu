package xiangshan.backend.regfile

import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import xiangshan.{ExuInput, Redirect, SrcType, XSBundle, XSModule}
import freechips.rocketchip.config.Parameters
import xiangshan.backend.issue.RsIdx
import xs.utils.Assertion.xs_assert

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

class GenericRegFile(entriesNum:Int, writeBackNum:Int, bypassNum:Int, readPortNum:Int, dataWidth:Int, moduleName:String, extraWriteNum:Int = 0, hasMask:Boolean = false)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val read = Vec(readPortNum, new ReadPort(dataWidth))
    val write = Vec(writeBackNum, new WritePort(dataWidth, hasMask))
    val bypassWrite = Vec(bypassNum, new WritePort(dataWidth, hasMask))
    val extraWrite = Vec(extraWriteNum, new WritePort(dataWidth, hasMask))
  })
  override val desiredName = moduleName
  println(s"${moduleName} read ports: $readPortNum regular write ports: $writeBackNum bypass write ports $bypassNum extra write ports $extraWriteNum")

  if(hasMask) {
    val bankNum = dataWidth / 8
    val mem = Mem(entriesNum, Vec(bankNum, UInt(8.W)))
    (io.write ++ io.bypassWrite ++ io.extraWrite).foreach(w => {
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
    val mem = Mem(entriesNum, UInt(dataWidth.W))
    (io.write ++ io.bypassWrite ++ io.extraWrite).foreach(w => {
      when(w.en) {
        mem.write(w.addr, w.data)
      }
    })

    io.read.foreach(r => {
      val memReadData = mem(r.addr)
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