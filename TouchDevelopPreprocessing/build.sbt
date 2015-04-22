scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test" / "scala"

resourceDirectory in Test := baseDirectory.value / "test" / "resources"

unmanagedJars in Compile <++= baseDirectory map { base =>
  val apronDirectories = base / ".." / "NumericalAnalysis" / "lib"
  val customJars = apronDirectories ** "*.jar"
  customJars.classpath
}

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.6-RC2"

// Make sure to use the same version of scalatest that was used to compile
// the SIL testing infrastructure class files to avoid IncompatibleClassErrors
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1"

libraryDependencies += "org.mongodb" %% "casbah" % "2.7.4"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "ch.qos.logback" % "logback-classic" % "1.1.2"
)



