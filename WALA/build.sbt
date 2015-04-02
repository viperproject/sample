scalaSource in Compile := baseDirectory.value / "src" / "scala"

resourceDirectory in Compile := baseDirectory.value / "src" / "resources"

scalaSource in Test := baseDirectory.value / "test" / "scala"

resourceDirectory in Test := baseDirectory.value / "test" / "resources"

unmanagedJars in Compile <++= baseDirectory map { base =>
  val libDir = base / "lib"
  val customJars = libDir ** "*.jar"
  customJars.classpath
}

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.6.4"
