import ch.ethz.inf.pm.td.transform.LoopRewriter
import ch.ethz.inf.pm.td.compiler.{Simplifier, TouchException, TouchCompiler}
import ch.ethz.inf.pm.td.parser.{PrettyPrinter, ScriptParser}
import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.webapi.{NewScripts, URLFetcher, NoMoreScriptsException}

/**
 *
 * Lucas Brutschy
 * Date: 8/23/12
 * Time: 6:09 PM
 *
 */

def compiler(url:String) {
  val url = script.getCodeURL
  println("\n\n===== Simplifying "+url)
  val ast = ScriptParser.parse(URLFetcher.fetchFile(url))
  println("=== ORIGINAL\n"+PrettyPrinter(ast))
  val sast = LoopRewriter(ast)
  println("=== SIMPLIFIED\n"+PrettyPrinter(sast))
}

TestRunner(new NewScripts,100,compiler)