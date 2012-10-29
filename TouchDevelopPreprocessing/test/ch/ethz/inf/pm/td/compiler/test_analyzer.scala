import ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraphExecution
import ch.ethz.inf.pm.sample.test.Run
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.webapi.NewScripts

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 3:59 PM
 *
 */

val comp = new TouchCompiler

def analyzer(url:String) {
  Run.main(List("-v","-p",url).toArray[String])
}

TestRunner(new NewScripts,3,analyzer)
