scalaSource in Compile <<= baseDirectory(_ / "src")

libraryDependencies += "commons-io" % "commons-io" % "2.0.1"