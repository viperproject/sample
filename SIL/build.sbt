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

unmanagedJars in Compile <++= baseDirectory map { base =>
  val apronDirectories = base / ".." / "NumericalAnalysis" / "lib"
  val customJars = apronDirectories ** "*.jar"
  customJars.classpath
}