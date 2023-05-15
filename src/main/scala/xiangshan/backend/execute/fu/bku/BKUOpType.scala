package xiangshan.backend.execute.fu.bku

import chisel3._
object BKUOpType {
  def clmul: UInt = "b000000".U
  def clmulh: UInt = "b000001".U
  def clmulr: UInt = "b000010".U
  def xpermn: UInt = "b000100".U
  def xpermb: UInt = "b000101".U
  def clz: UInt = "b001000".U
  def clzw: UInt = "b001001".U
  def ctz: UInt = "b001010".U
  def ctzw: UInt = "b001011".U
  def cpop: UInt = "b001100".U
  def cpopw: UInt = "b001101".U
  // 01xxxx is reserve
  def aes64es: UInt = "b100000".U
  def aes64esm: UInt = "b100001".U
  def aes64ds: UInt = "b100010".U
  def aes64dsm: UInt = "b100011".U
  def aes64im: UInt = "b100100".U
  def aes64ks1i: UInt = "b100101".U
  def aes64ks2: UInt = "b100110".U
  // merge to two instruction sm4ks & sm4ed
  def sm4ed0: UInt = "b101000".U
  def sm4ed1: UInt = "b101001".U
  def sm4ed2: UInt = "b101010".U
  def sm4ed3: UInt = "b101011".U
  def sm4ks0: UInt = "b101100".U
  def sm4ks1: UInt = "b101101".U
  def sm4ks2: UInt = "b101110".U
  def sm4ks3: UInt = "b101111".U
  def sha256sum0: UInt = "b110000".U
  def sha256sum1: UInt = "b110001".U
  def sha256sig0: UInt = "b110010".U
  def sha256sig1: UInt = "b110011".U
  def sha512sum0: UInt = "b110100".U
  def sha512sum1: UInt = "b110101".U
  def sha512sig0: UInt = "b110110".U
  def sha512sig1: UInt = "b110111".U
  def sm3p0: UInt = "b111000".U
  def sm3p1: UInt = "b111001".U
}
