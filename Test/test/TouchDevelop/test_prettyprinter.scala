import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.parser.PrettyPrinter
import ch.ethz.inf.pm.td.webapi.{WebASTImporter, ScriptListings}

/**
 *
 * Lucas Brutschy
 * Date: 8/13/12
 * Time: 3:36 PM
 *
 */

def prnt(id:String) {
  val s = WebASTImporter.queryAndConvert(id)
  println(s)
  println(PrettyPrinter(s))
}

TestRunner(new ScriptListings,100000,prnt _)

