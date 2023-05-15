package xiangshan.backend.execute.fu

import chisel3._
import xiangshan.ExceptionNO._
import xiangshan.FuType
case class FuConfig
(
  name: String,
  fuType: UInt,
  numIntSrc: Int,
  numFpSrc: Int,
  numVecSrc:Int,
  writeIntRf: Boolean,
  writeFpRf: Boolean,
  writeVecRf: Boolean,
  writeFflags: Boolean,
  latency: Int,
  hasRedirect: Boolean,
  exceptionOut: Seq[Int] = Seq(),
) {
  def srcCnt: Int = math.max(numIntSrc, numFpSrc)
  override def toString = name
}

object FuConfigs{
  val aluCfg = FuConfig(
    name = "alu",
    fuType = FuType.alu,
    numIntSrc = 2,
    numFpSrc = 0,
    numVecSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = false,
    latency = 0,
    hasRedirect = true
  )
  val mulCfg = FuConfig(
    name = "mul",
    fuType = FuType.mul,
    numIntSrc = 2,
    numFpSrc = 0,
    numVecSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = false,
    latency = 1, //Actual latency is 2. It is reduced to 1 because of bypass network
    hasRedirect = false
  )
  val bkuCfg = FuConfig(
    name = "bku",
    fuType = FuType.bku,
    numIntSrc = 2,
    numFpSrc = 0,
    numVecSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = false,
    latency = 1, //Actual latency is 2. It is reduced to 1 because of bypass network
    hasRedirect = false
  )
  val divCfg = FuConfig(
    name = "div",
    FuType.div,
    numIntSrc = 2,
    numFpSrc = 0,
    numVecSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = false,
    latency = Int.MaxValue,
    hasRedirect = false
  )
  val jmpCfg = FuConfig(
    name = "jmp",
    fuType = FuType.jmp,
    numIntSrc = 1,
    numFpSrc = 0,
    numVecSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = false,
    latency = 0,
    hasRedirect = true
  )
  val fenceCfg = FuConfig(
    name = "fence",
    fuType = FuType.fence,
    numIntSrc = 2,
    numFpSrc = 0,
    numVecSrc = 0,
    writeIntRf = false,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = false,
    latency = Int.MaxValue,
    hasRedirect = false,
    exceptionOut = Seq(illegalInstr)
  )
  val csrCfg = FuConfig(
    name = "csr",
    fuType = FuType.csr,
    numIntSrc = 1,
    numFpSrc = 0,
    numVecSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = false,
    latency = Int.MaxValue,
    hasRedirect = false,
    exceptionOut = Seq(illegalInstr, breakPoint, ecallU, ecallS, ecallM)
  )
  val i2fCfg = FuConfig(
    name = "i2f",
    fuType = FuType.i2f,
    numIntSrc = 1,
    numFpSrc = 0,
    numVecSrc = 0,
    writeIntRf = false,
    writeFpRf = true,
    writeVecRf = false,
    writeFflags = true,
    latency = 2,
    hasRedirect = false
  )
  val fmacCfg = FuConfig(
    name = "fmac",
    fuType = FuType.fmac,
    numIntSrc = 0,
    numFpSrc = 3,
    numVecSrc = 0,
    writeIntRf = false,
    writeFpRf = true,
    writeVecRf = false,
    writeFflags = true,
    latency = Int.MaxValue,
    hasRedirect = false
  )

  val f2iCfg = FuConfig(
    name = "f2i",
    fuType = FuType.fmisc,
    numIntSrc = 0,
    numFpSrc = 1,
    numVecSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = true,
    latency = 2,
    hasRedirect = false
  )

  val f2fCfg = FuConfig(
    name = "f2f",
    fuType = FuType.fmisc,
    numIntSrc = 0,
    numFpSrc = 1,
    numVecSrc = 0,
    writeIntRf = false,
    writeFpRf = true,
    writeVecRf = false,
    writeFflags = true,
    latency = 2,
    hasRedirect = false
  )

  val fdivSqrtCfg = FuConfig(
    name = "fdivSqrt",
    fuType = FuType.fDivSqrt,
    numIntSrc = 0,
    numFpSrc = 2,
    numVecSrc = 0,
    writeIntRf = false,
    writeFpRf = true,
    writeVecRf = false,
    writeFflags = true,
    latency = Int.MaxValue,
    hasRedirect = false
  )

  val lduCfg = FuConfig(
    name = "ldu",
    fuType = FuType.ldu,
    numIntSrc = 1,
    numFpSrc = 0,
    numVecSrc = 1,
    writeIntRf = true,
    writeFpRf = true,
    writeVecRf = true,
    writeFflags = false,
    latency = Int.MaxValue,
    hasRedirect = true,
    exceptionOut = Seq(loadAddrMisaligned, loadAccessFault, loadPageFault),
  )

  val staCfg = FuConfig(
    name = "sta",
    fuType = FuType.stu,
    numIntSrc = 1,
    numFpSrc = 0,
    numVecSrc = 1,
    writeIntRf = false,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = false,
    latency = Int.MaxValue,
    hasRedirect = true,
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault)
  )

  val stdCfg = FuConfig(
    name = "std",
    fuType = FuType.stu,
    numIntSrc = 1,
    numFpSrc = 1,
    numVecSrc = 1,
    writeIntRf = false,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = false,
    latency = Int.MaxValue,
    hasRedirect = false
  )

  val mouCfg = FuConfig(
    name = "mou",
    fuType = FuType.mou,
    numIntSrc = 2,
    numFpSrc = 0,
    numVecSrc = 0,
    writeIntRf = false,
    writeFpRf = false,
    writeVecRf = false,
    writeFflags = false,
    latency = Int.MaxValue,
    hasRedirect = false,
    exceptionOut = lduCfg.exceptionOut ++ staCfg.exceptionOut
  )
}