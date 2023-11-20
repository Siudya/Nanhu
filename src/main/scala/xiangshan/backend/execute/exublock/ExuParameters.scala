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
package xiangshan.backend.execute.exublock

case class ExuParameters
(
  JmpCnt:Int = 1,
  AluMiscCnt:Int = 1,
  AluMulCnt:Int = 2,
  AluDivCnt:Int = 1,
  FmaCnt:Int = 2,
  FmaDivCnt:Int = 1,
  FmaMiscCnt:Int = 1,
  LduCnt:Int = 2,
  StuCnt:Int = 2
){
  val aluNum:Int = AluMulCnt + AluDivCnt + AluMiscCnt
  val mulNum:Int = AluMulCnt
  val LsExuCnt:Int = LduCnt + StuCnt
  val fmaNum:Int = FmaCnt + FmaDivCnt + FmaMiscCnt
}
