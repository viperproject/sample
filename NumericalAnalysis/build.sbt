scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test" / "scala")

resourceDirectory in Test <<= baseDirectory(_ / "test" / "resources")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test"
