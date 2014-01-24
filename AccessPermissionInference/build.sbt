scalaSource in Compile <<= baseDirectory(_ / "src")

// Don't do that for now, as intellij tries to compile test files which should be resources only
// resourceDirectory in Test <<= baseDirectory(_ / "test" / "resources")
