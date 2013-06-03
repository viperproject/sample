import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi._

def analyzer(id:String) {
  TestRunner.runIdWithApron(id)
}

TestRunner("uzbezvid",analyzer _)
