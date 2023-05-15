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
  val LsExuCnt:Int = LduCnt + StuCnt
  val specWakeUpNum:Int = AluMulCnt * 2 + AluDivCnt + AluJmpCnt
}
