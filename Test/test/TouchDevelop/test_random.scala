import ch.ethz.inf.pm.td.analysis.TestRunner

val skipSet =
  """
  """.stripMargin.split("\n").map(_.trim).toSet

def analyzer(id:String) {
  if(!skipSet.contains(id)) {
    TestRunner.runIdWithApron(id)
  }
}

TestRunner("aecczdxr",analyzer _)
//TestRunner(new NewScripts,10000,analyzer _)
