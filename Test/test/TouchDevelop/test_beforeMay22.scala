import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi._
import java.util.GregorianCalendar

val skipSet =
  """
  """.stripMargin.split("\n").map(_.trim).toSet

def analyzer(id:String) {
  if(!skipSet.contains(id)) {
    TestRunner.runIdWithApron(id)
  }
}

TestRunner(new ScriptsBefore((new GregorianCalendar(2013,04,22)).getTime),10000,analyzer _)
