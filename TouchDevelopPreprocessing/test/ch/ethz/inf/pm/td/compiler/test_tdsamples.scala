import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi.{RootSampleScripts, SampleScript}

val skipSet =
  """ucbc
    |jvuua
  """.stripMargin.split("\n").map(_.trim).toSet

def analyzer(id:String) {
  if(!skipSet.contains(id)) {
    TestRunner.runIdWithApron(id)
  }
}

//TestRunner("fqap",analyzer _)
//TestRunner("avvj",analyzer _)
TestRunner(new RootSampleScripts,10000,analyzer)
