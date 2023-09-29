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

import mill._
import scalalib._
import scalafmt._
import os.Path
import publish._
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.build

val defaultVersions = Map(
  "chisel" -> "6.0.0-M3",
  "chisel-plugin" -> "6.0.0-M3",
  "chiseltest" -> "5.0.0",
  "scala" -> "2.13.10",
  "scalatest" -> "3.2.7"
)

def getVersion(dep: String, org: String = "org.chipsalliance", cross: Boolean = false) = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  if (cross)
    ivy"$org:::$dep:$version"
  else
    ivy"$org::$dep:$version"
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = defaultVersions("scala")

  override def scalacPluginIvyDeps = Agg(getVersion("chisel-plugin", cross = true))

  override def scalacOptions = super.scalacOptions() ++ Agg("-Ytasty-reader", "-Ymacro-annotations")

}

object rocketchip extends RocketChip

trait RocketChip
  extends millbuild.`rocket-chip`.common.RocketChipModule
    with SbtModule {
  def scalaVersion: T[String] = T(defaultVersions("scala"))

  override def millSourcePath = os.pwd / "rocket-chip"

  def chiselModule = None

  def chiselPluginJar = None

  def chiselIvy = Some(getVersion("chisel"))

  def chiselPluginIvy = Some(getVersion("chisel-plugin", cross=true))

  def macrosModule = macros

  def hardfloatModule = hardfloat

  def cdeModule = cde

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.5.0"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.5"

  object macros extends Macros

  trait Macros
    extends millbuild.`rocket-chip`.common.MacrosModule
      with SbtModule {

    def scalaVersion: T[String] = T(defaultVersions("scala"))

    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultVersions("scala")}"
  }

  object hardfloat extends Hardfloat

  trait Hardfloat
    extends millbuild.`rocket-chip`.hardfloat.common.HardfloatModule {

    def scalaVersion: T[String] = T(defaultVersions("scala"))

    override def millSourcePath = os.pwd / "rocket-chip" / "hardfloat" / "hardfloat"

    def chiselModule = None

    def chiselPluginJar = None

    def chiselIvy = Some(getVersion("chisel"))

    def chiselPluginIvy = Some(getVersion("chisel-plugin", cross=true))
  }

  object cde extends CDE

  trait CDE
    extends millbuild.`rocket-chip`.cde.common.CDEModule
      with ScalaModule {

    def scalaVersion: T[String] = T(defaultVersions("scala"))

    override def millSourcePath = os.pwd / "rocket-chip" / "cde" / "cde"
  }
}

object difftest extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(getVersion("chisel"))

  override def millSourcePath = os.pwd / "difftest"
}

object fudian extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(getVersion("chisel"))

  override def millSourcePath = os.pwd / "fudian"
}

object xsutils extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(getVersion("chisel"))

  override def millSourcePath = os.pwd / "xs-utils"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip
  )
}

object huancun extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(getVersion("chisel"))

  override def millSourcePath = os.pwd / "huancun"

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip, xsutils)
}

object axi2tl extends CommonModule with SbtModule {

  override def millSourcePath = os.pwd / "coupledL2" / "AXItoTL"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip,
    xsutils
  )
}

object coupledL2 extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(getVersion("chisel"))

  override def millSourcePath = os.pwd / "coupledL2"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip,
    huancun,
    xsutils,
    axi2tl
  )
}

object XiangShan extends SbtModule with ScalafmtModule with CommonModule {

  override def millSourcePath = millOuterCtx.millSourcePath

  override def forkArgs = Seq("-Xmx64G", "-Xss256m")

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel"),
    getVersion("chiseltest", "edu.berkeley.cs"),
  )

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip,
    xsutils,
    huancun,
    difftest,
    coupledL2,
    fudian,
    axi2tl
  )

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      getVersion("scalatest","org.scalatest")
    )

    def testFramework = "org.scalatest.tools.Framework"
  }
}