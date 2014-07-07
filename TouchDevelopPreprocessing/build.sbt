scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test/scala")

resourceDirectory in Test <<= baseDirectory(_ / "test/resources")

unmanagedJars in Compile <++= baseDirectory map { base =>
            val additionalDirectories = (base / "../SIL/lib") 
            val customJars = (additionalDirectories ** "*.jar") 
            customJars.classpath
}

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"

libraryDependencies += "nu.validator.htmlparser" % "htmlparser" % "1.4"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.5"

libraryDependencies += "com.thoughtworks.paranamer" % "paranamer" % "2.5.2"

// Make sure to use the same version of scalatest that was used to compile
// the SIL testing infrastructure class files to avoid IncompatibleClassErrors
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1"

libraryDependencies += "org.mongodb" %% "casbah" % "2.6.0"

