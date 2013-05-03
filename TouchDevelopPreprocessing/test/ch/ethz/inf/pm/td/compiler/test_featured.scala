import ch.ethz.inf.pm.sample.test.Run
import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.webapi._

val skipSet =
  """
  """.stripMargin.split("\n").map(_.trim).toSet

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 3:59 PM
 *
 */

def analyzer(id:String) {
  if(!skipSet.contains(id)) {
    TestRunner.runIdWithApron(id)
  }
}

//TestRunner("julr",analyzer _)
TestRunner(new TopScripts,10000,analyzer)
