scalaSource in Compile := baseDirectory.value / "src"

resourceDirectory in Test := baseDirectory.value / "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1"

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.34"

