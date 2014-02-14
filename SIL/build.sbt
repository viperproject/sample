// Make sure to use the same version of scalatest that was used to compile
// the SIL testing infrastructure class files to avoid IncompatibleClassErrors
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1"

// SIL dependency (not part of the SIL JAR)
libraryDependencies += "com.googlecode.kiama" % "kiama_2.10" % "1.5.1"

// SIL dependency (not part of the SIL JAR)
libraryDependencies += "org.rogach" %% "scallop" % "0.9.4"

// SIL dependency (not part of the SIL JAR)
libraryDependencies += "org.jgrapht" % "jgrapht-jdk1.5" % "0.7.3"