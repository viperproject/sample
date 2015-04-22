unmanagedResourceDirectories in Compile += baseDirectory.value / ".." / "SIL" / "src" / "test" / "resources"
unmanagedResourceDirectories in Compile += baseDirectory.value / ".." / "TouchDevelopPreprocessing" / "test" / "resources"

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.3.0",
  "org.scalatra" %% "scalatra-specs2" % "2.3.0" % "test;compile",
  "ch.qos.logback" % "logback-classic" % "1.0.6" % "runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "9.2.1.v20140609" % "container;compile",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container;provided;test" artifacts Artifact("javax.servlet", "jar", "jar")
)

