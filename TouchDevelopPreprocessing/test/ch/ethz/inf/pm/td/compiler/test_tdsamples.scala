import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi.SampleScript

val skipSet =
  """ucbc
    |jvuua
  """.stripMargin.split("\n").map(_.trim).toSet

def analyzer(id:String) {
  if(!skipSet.contains(id)) {
    TestRunner.runIdWithApron(id)
  }
}

//TestRunner("wbop",analyzer _)
TestRunner("yzty",analyzer _)
//TestRunner(new SampleScript,10000,analyzer)
