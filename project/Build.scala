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
    base = file(".")) aggregate(sample_core, sample_numerical, sample_apron,
    sample_touchdevelop, sample_scala_processing, sample_partitioning,
    sample_string, sample_valuedriven_heap, sample_sil, sample_web,
    sample_silver)

  lazy val sample_core = Project(
    id = "sample-core",
    base = file("sample"))

  lazy val sample_numerical = Project(
    id = "sample-numerical",
    base = file("NumericalAnalysis")
  ) dependsOn sample_core

  lazy val sample_apron = Project(
    id = "sample-apron",
    base = file("Apron")
  ) dependsOn(sample_core, sample_numerical)

  lazy val sample_touchdevelop = Project(
    id = "sample-touchdevelop",
    base = file("TouchDevelopPreprocessing")
  ) dependsOn(sample_core, sample_numerical, sample_apron, sample_string)

  lazy val sample_scala_processing = Project(
    id = "sample-scala-preprocessing",
    base = file("ScalaPreprocessing")
  ) dependsOn sample_core

  lazy val sample_partitioning = Project(
    id = "sample-partitioning",
    base = file("Partitioning")
  ) dependsOn(sample_core, sample_scala_processing)

  lazy val sample_string = Project(
    id = "sample-string",
    base = file("String")
  ) dependsOn(sample_core, sample_numerical, sample_apron)

  lazy val sample_valuedriven_heap = Project(
    id = "sample-valuedriven-heap",
    base = file("ValueDrivenHeapAnalysis")
  ) dependsOn(sample_core, sample_numerical, sample_apron, sample_scala_processing)

  lazy val sample_sil = Project(
    id = "sample-sil",
    base = file("SIL")
  ) dependsOn(sample_core, sample_numerical, sample_apron, sample_valuedriven_heap)

  lazy val sample_silver = Project(
    id = "sample-silver",
    base = file("sample-silver")
  ) dependsOn(sample_core, sample_numerical, silver, silicon, carbon)

  lazy val sample_web = Project(
    id = "sample-web",
    base = file("Web"),
    settings = Defaults.coreDefaultSettings ++ com.earldouglas.xwp.XwpPlugin.jetty()
  ) enablePlugins(SbtTwirl) dependsOn(sample_core, sample_numerical, sample_apron, sample_valuedriven_heap, sample_sil, sample_touchdevelop)

  lazy val silver = RootProject(
    file("../silver/")
  )

  lazy val silicon = RootProject(
    file("../silicon/")
  )

  lazy val carbon = RootProject(
    file("../carbon/")
  )

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
      "ch.qos.logback" % "logback-classic" % "1.1.7" % "runtime", // Logging Backend % "runtime"
      "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0", // Logging Frontend
      "org.scalatest" % "scalatest_2.11" % "2.2.1", // Testing Framework
      "org.scalaz" %% "scalaz-core" % "7.1.5" // Functional Programming
    )
  )
}
