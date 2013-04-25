import ch.ethz.inf.pm.sample.test.Run
import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.webapi.NewScripts

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 3:59 PM
 *
 */

def analyzer(id:String) {
  TestRunner.runIdWithApron(id)
}

TestRunner(new NewScripts,100,analyzer)
