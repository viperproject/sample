import ch.ethz.inf.pm.td.analysis.TestRunner

val toAnalyze =
  """
  """.stripMargin.split("[\n,]").map(_.trim).toSet

def analyzer(id:String) {
  if(!id.isEmpty)
    TestRunner.runIdWithApron(id)
}

for(a <- toAnalyze) {
  analyzer(a)
}

TestRunner("htmh",analyzer _)
TestRunner("nyud",analyzer _)
TestRunner("lypy",analyzer _)
TestRunner("hxvm",analyzer _)
TestRunner("kfpe",analyzer _)
TestRunner("veuo",analyzer _)
TestRunner("aohm",analyzer _)
TestRunner("boeg",analyzer _)
TestRunner("gxhi",analyzer _)
TestRunner("prlg",analyzer _)
TestRunner("thyz",analyzer _)
TestRunner("unrr",analyzer _)
TestRunner("zwqc",analyzer _)
