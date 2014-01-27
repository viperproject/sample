scalaSource in Compile <<= baseDirectory(_ / "src")

// The following is problematic: It makes the intellij compilation fail
// because the test _resources_ are treated as source files (the .scala
// resources may not be valid)
// resourceDirectory in Test <<= baseDirectory(_ / "test")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1"

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.16"