scalaSource in Compile := baseDirectory.value / "src"

// dont do that for now, as intellij tries to compile test files which should be resources only
// resourceDirectory in Test := baseDirectory.value / "test" / "resources"
