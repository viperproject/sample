scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test" / "scala"

resourceDirectory in Test := baseDirectory.value / "test" / "resources"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

