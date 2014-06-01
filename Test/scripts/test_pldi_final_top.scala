
import ch.ethz.inf.pm.td.analysis.TestRunner

val toAnalyze =
  """ajkc
    |dvvx
    |htmh
    |kblp
    |kmjn
    |mpuj
    |qwzu
    |wbxsa
    |ybcy
    |zids
  """.stripMargin.split("[\n,]").map(_.trim)

def analyzer(id:String) {
  if (!id.isEmpty)
    TestRunner.runIdWithApron(id)
}

for(a <- toAnalyze) {
  analyzer(a)
}
