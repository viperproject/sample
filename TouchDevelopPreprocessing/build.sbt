scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test" / "scala"

resourceDirectory in Test := baseDirectory.value / "test" / "resources"

unmanagedJars in Compile <++= baseDirectory map { base =>
  val apronDirectories = base / ".." / "NumericalAnalysis" / "lib"
  val customJars = apronDirectories ** "*.jar"
  customJars.classpath
}

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.6.2"

libraryDependencies += "org.mongodb" %% "casbah" % "3.0.0"

libraryDependencies += "com.novus" %% "salat" % "1.9.9"
