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

package xiangshan.backend.execute.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.ExceptionNO.fdiUJumpFault
import xiangshan.backend.execute.fu.csr.{CSR, CSRFileIO, CSROpType}
import xiangshan.backend.execute.fu.fence.{SfenceBundle, _}
import xiangshan.backend.execute.fu.jmp._
import xiangshan.backend.execute.fu.{FUWithRedirect, FuConfigs, FunctionUnit, FDICallJumpExcpIO}
import xiangshan._
import xs.utils.{DelayN, ParallelMux}

class FakeStd()(implicit p: Parameters) extends FunctionUnit(p(XSCoreParamsKey).XLEN) {
  io.out <> io.in
}

class FakeMISC(implicit p: Parameters) extends FunctionUnit(p(XSCoreParamsKey).XLEN) {
  val issueToMisc = IO(Decoupled(new ExuInput))
  val writebackFromMisc = IO(Flipped(Decoupled(new ExuOutput)))
  io.in.ready := issueToMisc.ready
  issueToMisc.valid := io.in.valid
  issueToMisc.bits := io.in.bits
  io.out.valid := writebackFromMisc.valid
  io.out.bits := writebackFromMisc.bits
  writebackFromMisc.ready := io.out.ready
}