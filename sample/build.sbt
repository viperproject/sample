scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test/scala"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test"
