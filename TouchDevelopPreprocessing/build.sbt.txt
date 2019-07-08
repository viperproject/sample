/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test" / "scala"

resourceDirectory in Test := baseDirectory.value / "test" / "resources"

unmanagedJars in Compile <++= baseDirectory map { base =>
  val apronDirectories = base / ".." / "NumericalAnalysis" / "lib"
  val customJars = apronDirectories ** "*.jar"
  customJars.classpath
}


libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.5"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.6.2"

libraryDependencies += "org.mongodb" %% "casbah" % "3.0.0"

libraryDependencies += "com.novus" %% "salat" % "1.9.9"
