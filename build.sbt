/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

lazy val sample = project
  .in(file("."))
  .aggregate(sample_qp)

lazy val sample_core = project
  .in(file("sample_core"))
  .settings(
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2", // Logging Frontend
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5", // Testing Framework
  )

lazy val sample_numerical = project
  .in(file("sample_numerical"))
  .dependsOn(sample_core)

lazy val sample_apron = project
  .in(file("sample_apron"))
  .dependsOn(sample_core)
  .dependsOn(sample_numerical)

lazy val sample_silver = project
  .in(file("sample_silver"))
  .dependsOn(sample_core)
  .dependsOn(sample_numerical)
  .dependsOn(sample_apron)
  .dependsOn(silver)
  .dependsOn(silicon)
  .dependsOn(carbon)
  .settings(
    libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.1", // Graph Library used for interproc call graphs
    libraryDependencies += "net.liftweb" %% "lift-json" % "3.3.0",
  )

lazy val sample_qp = project
  .in(file("sample_qp"))
  .dependsOn(sample_silver)
  .settings(
    // general settings
    name := "sample_qp",
    organization := "viper",
    version := "0.1-SNAPSHOT",
  )

lazy val silver = project
  .in(file("silver"))

lazy val silicon = project
  .in(file("silicon"))

lazy val carbon = project
  .in(file("carbon"))