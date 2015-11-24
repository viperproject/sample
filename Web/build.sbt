unmanagedResourceDirectories in Compile += baseDirectory.value / ".." / "SIL" / "src" / "test" / "resources"
unmanagedResourceDirectories in Compile += baseDirectory.value / ".." / "TouchDevelopPreprocessing" / "test" / "resources"


libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.3.1",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

unmanagedJars in Compile <++= baseDirectory map { base =>
  val apronDirectories = base / ".." / "NumericalAnalysis" / "lib"
  val silDirectories = base / ".." / "SIL" / "lib"
  val customJars = (apronDirectories ** "*.jar") +++ (silDirectories ** "*.jar")
  customJars.classpath
}