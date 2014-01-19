scalaSource in Compile := baseDirectory.value / "src"

// the following is problematic: it makes the intellij compilation fail
// because the test _resources_ are treated as source files (the .scala
// resources may not be valid)
//resourceDirectory in Test := baseDirectory.value / "test"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test"

