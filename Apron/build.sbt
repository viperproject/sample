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
  val apronDirectories = base / "lib"
  val customJars = apronDirectories ** "*.jar"
  customJars.classpath
}