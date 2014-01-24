scalaSource in Compile <<= baseDirectory(_ / "src")

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.3"
