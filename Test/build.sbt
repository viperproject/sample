scalaSource in Compile := baseDirectory.value / "src"

resourceDirectory in Test := baseDirectory.value / "test"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test"

