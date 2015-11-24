unmanagedResourceDirectories in Compile += baseDirectory.value / ".." / "SIL" / "src" / "test" / "resources"
unmanagedResourceDirectories in Compile += baseDirectory.value / ".." / "TouchDevelopPreprocessing" / "test" / "resources"


libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.3.1",
  "org.scalatra" %% "scalatra-specs2" % "2.3.1" % "test",
  "org.eclipse.jetty" % "jetty-webapp" % "9.2.10.v20150310" % "container;compile",
  "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container;provided;test" artifacts Artifact("javax.servlet", "jar", "jar"),
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

unmanagedJars in Compile <++= baseDirectory map { base =>
  val apronDirectories = base / ".." / "NumericalAnalysis" / "lib"
  val silDirectories = base / ".." / "SIL" / "lib"
  val customJars = (apronDirectories ** "*.jar") +++ (silDirectories ** "*.jar")
  customJars.classpath
}

