import ch.ethz.inf.pm.td.parser.{PrettyPrinter, Script, ScriptParser}
import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.webapi.URLFetcher

/**
 *
 * Lucas Brutschy
 * Date: 8/13/12
 * Time: 3:36 PM
 *
 */

def prnt(url:String) {
  val s = ScriptParser(URLFetcher.fetchFile(url))
  println(s)
  println(PrettyPrinter(s))
}

TestRunner("https://www.touchdevelop.com/api/qssc/text",prnt)
