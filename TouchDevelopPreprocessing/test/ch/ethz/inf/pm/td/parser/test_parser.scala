import ch.ethz.inf.pm.td.compiler.TouchException
import ch.ethz.inf.pm.td.parser.{PrettyPrinter, Script, ScriptParser}
import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.webapi.{Scripts, NoMoreScriptsException, FeaturedScripts, URLFetcher}

/**
 *
 * Lucas Brutschy
 * Date: 8/13/12
 * Time: 3:36 PM
 *
 */
def prnt(url:String) {
  println(url)
  val s = ScriptParser(URLFetcher.fetchFile(url))
  //println(s)
  //println(PrettyPrinter(s))
}

TestRunner(new Scripts,1000000,prnt)
