
import ch.ethz.inf.pm.td.analysis.TestRunner

val toAnalyze =
  """gwsfmlau
    |blwd
    |eeorzcap
    |jwgqbjzs
    |jwnw
  """.stripMargin.split("[\n,]").map(_.trim)

def analyzer(id: String) {
  if (!id.isEmpty)
    TestRunner.runIdWithApron(id)
}

for (a <- toAnalyze) {
  analyzer(a)
}
