scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test/scala")

resourceDirectory in Test <<= baseDirectory(_ / "test/resources")

unmanagedBase <<= baseDirectory(_ / "../SIL/lib")

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"

libraryDependencies += "nu.validator.htmlparser" % "htmlparser" % "1.4"

libraryDependencies += "net.liftweb" % "lift-json_2.10" % "2.5-M4"

libraryDependencies += "com.thoughtworks.paranamer" % "paranamer" % "2.5.2"

// Make sure to use the same version of scalatest that was used to compile
// the SIL testing infrastructure class files to avoid IncompatibleClassErrors
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1"
