scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test" / "scala")

// In SBT 0.13.x, the syntax for sources custom source directories is as follows:
// scalaSource in Compile := baseDirectory.value / "src"
// When porting to SBT 0.13.x, also change the other build.sbt files

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test"
