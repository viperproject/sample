import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.parser.PrettyPrinter
import ch.ethz.inf.pm.td.webapi._

/**
 *
 * Lucas Brutschy
 * Date: 8/13/12
 * Time: 3:36 PM
 *
 */
def prnt(id:String) {
  println(id)
  val japp = WebASTImporter.query(id)
  println(japp.toString)
  val convert = WebASTImporter.convert(japp)
  println(convert)
  println(PrettyPrinter(convert))
}

TestRunner(new ScriptQuery,1000000,prnt _)
