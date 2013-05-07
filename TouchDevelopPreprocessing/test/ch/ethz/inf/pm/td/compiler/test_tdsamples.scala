import ch.ethz.inf.pm.sample.test.Run
import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.webapi.{SampleScript, RootScripts, Scripts, NewScripts}

val skipSet =
  """ucbc
    |jvuua
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

TestRunner("zgve",analyzer _)
//TestRunner(new SampleScript,10000,analyzer)
