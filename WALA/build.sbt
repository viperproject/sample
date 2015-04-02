scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test" / "scala"

resourceDirectory in Test := baseDirectory.value / "test" / "resources"

unmanagedJars in Compile <++= baseDirectory map { base =>
  val libDir = base / "lib"
  val customJars = libDir ** "*.jar"
  customJars.classpath
}

