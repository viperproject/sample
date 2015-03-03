scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test" / "scala"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"