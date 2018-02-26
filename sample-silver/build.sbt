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

// Breeze@ScalaNLP: provides a DSL for solving linear programs, using Apache's Simplex Solver as the backend
libraryDependencies += "org.scalanlp" %% "breeze" % "0.11.2" withSources()

libraryDependencies += "net.liftweb" %% "lift-json" % "3.1.0" // Json Library for IDE