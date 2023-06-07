package xiangshan.backend.execute.exublock

case class ExuParameters
(
  AluJmpCnt:Int = 1,
  AluMulCnt:Int = 2,
  AluDivCnt:Int = 1,
  FmaCnt:Int = 2,
  FmaDivCnt:Int = 1,
  FmaMiscCnt:Int = 1,
  LduCnt:Int = 2,
  StuCnt:Int = 2
){
  val aluNum:Int = AluMulCnt + AluDivCnt + AluJmpCnt
  val mulNum:Int = AluMulCnt
  val LsExuCnt:Int = LduCnt + StuCnt
  val specWakeUpNum:Int = aluNum * 2 + mulNum
}
