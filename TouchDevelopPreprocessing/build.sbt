resolvers += "Choco solver repo" at "http://www.emn.fr/z-info/choco-repo/mvn/repository/"

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

libraryDependencies += "net.liftweb" % "lift-json_2.10" % "2.5-M4"

libraryDependencies += "com.thoughtworks.paranamer" % "paranamer" % "2.5.2"

libraryDependencies += "choco" % "choco-solver" % "3.1.0"

// Make sure to use the same version of scalatest that was used to compile
// the SIL testing infrastructure class files to avoid IncompatibleClassErrors
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "0.8.0"