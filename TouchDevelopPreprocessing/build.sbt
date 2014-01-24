scalaSource in Compile <<= baseDirectory(_ / "src")

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"

libraryDependencies += "nu.validator.htmlparser" % "htmlparser" % "1.4"

libraryDependencies += "net.liftweb" % "lift-json_2.10" % "2.5-M4"

libraryDependencies += "com.thoughtworks.paranamer" % "paranamer" % "2.5.2"