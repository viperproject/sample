import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi.{ScriptQuery, WebASTImporter}

/**
 * User: lucas
 * Date: 11/22/12
 * Time: 5:50 PM
 */

def prnt(id:String) {
  println(id)
  val japp = WebASTImporter.query(id)
  println(japp.toString)
}

prnt("dmwm")
TestRunner(new ScriptQuery,1000000,prnt _)
