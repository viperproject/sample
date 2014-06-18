
import ch.ethz.inf.pm.td.analysis.TestRunner

val toAnalyze =
  """kblp
    |htmh
    |kmjn
    |mpuj
    |qwzu
    |wbxsa
    |ybcy
    |zids
    |ajkc
    |dvvx
  """.stripMargin.split("[\n,]").map(_.trim)

def analyzer(id: String) {
  if (!id.isEmpty)
    TestRunner.runIdWithApron(id)
}

for (a <- toAnalyze) {
  analyzer(a)
}
