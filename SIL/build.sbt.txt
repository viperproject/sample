/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

// Make sure to use the same version of scalatest that was used to compile
// the SIL testing infrastructure class files to avoid IncompatibleClassErrors

// SIL dependency (not part of the SIL JAR)
libraryDependencies += "com.googlecode.kiama" % "kiama_2.11" % "1.8.0"

// SIL dependency (not part of the SIL JAR)
libraryDependencies += "org.rogach" %% "scallop" % "0.9.5"

// SIL dependency (not part of the SIL JAR)
libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.0"

// SIL dependency (not part of the SIL JAR)
libraryDependencies += "org.jgrapht" % "jgrapht-ext" % "0.9.0"
