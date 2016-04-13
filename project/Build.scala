/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

import sbt._
import sbt.Keys._
import play.twirl.sbt.SbtTwirl

object SampleBuild extends Build {
  lazy val root = Project(
    id = "sample",
    base = file(".")) aggregate(core, numerical, touchdevelop,
    scalapreproc, partitioning, string, valuedrivenheap, sil, silver, web)

  lazy val core = Project(
    id = "sample-core",
    base = file("sample"))

  lazy val numerical = Project(
    id = "sample-numerical",
    base = file("NumericalAnalysis")) dependsOn(core)

  lazy val touchdevelop = Project(
    id = "sample-touchdevelop",
    base = file("TouchDevelopPreprocessing")) dependsOn(core, numerical, string)

  lazy val scalapreproc = Project(
    id = "sample-scala-preprocessing",
    base = file("ScalaPreprocessing")) dependsOn core

  lazy val partitioning = Project(
    id = "sample-partitioning",
    base = file("Partitioning")) dependsOn(core, scalapreproc)

  lazy val string = Project(
    id = "sample-string",
    base = file("String")) dependsOn(core, numerical)

  lazy val valuedrivenheap = Project(
    id = "sample-valuedriven-heap",
    base = file("ValueDrivenHeapAnalysis")) dependsOn(core, numerical, scalapreproc)

  lazy val sil = Project(
    id = "sample-sil",
    base = file("SIL")) dependsOn(core, numerical, valuedrivenheap)

  lazy val silver = Project(
    id = "sample-silver",
    base = file("Silver")) dependsOn(core, numerical, valuedrivenheap)

  lazy val web = Project(
    id = "sample-web",
    base = file("Web"),
    settings = Defaults.coreDefaultSettings++ com.earldouglas.xwp.XwpPlugin.jetty()
  ).enablePlugins(SbtTwirl).dependsOn(core, numerical, valuedrivenheap, sil, touchdevelop)

  // Custom configuration key to specify apron shared library location
  lazy val apronLibPath = SettingKey[String]("apronLibPath",
    "Absolute path with directory containing apron native libraries")

  // Global settings across sub-projects
  override lazy val settings = super.settings ++ Seq(
    /*
     * Unfortunately we need this because of apron. Need to fork JVM to avoid
     * native library loading issues when running e.g. tests a second time in sbt
     *
     * Also, set the native library path
     */
    fork := true,
    javaOptions <+= apronLibPath map {
      p => "-Djava.library.path=" + p
    },
    scalacOptions in Compile ++= Seq(
      "-optimise",
      "-unchecked",
      "-deprecation",
      "-feature"
      //"-Xelide-below", "3000",
      //"-Xdisable-assertions"
    ),
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.1.3" % "runtime", // Logging Backend
      "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",  // Logging Frontend
      "org.scalatest" % "scalatest_2.11" % "2.2.1",               // Testing Framework
      "org.scalaz" %% "scalaz-core" % "7.1.5"                     // Functional Programming
    )
  )

}
