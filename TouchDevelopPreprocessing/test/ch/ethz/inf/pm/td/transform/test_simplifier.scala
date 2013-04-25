import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.transform.LoopRewriter
import ch.ethz.inf.pm.td.parser.{PrettyPrinter, ScriptParser}
import ch.ethz.inf.pm.td.typecheck.Typer
import ch.ethz.inf.pm.td.webapi.{WebASTImporter, Scripts, NewScripts, URLFetcher}

/**
 *
 * Lucas Brutschy
 * Date: 8/23/12
 * Time: 6:09 PM
 *
 */

def compiler(id:String) {
  println("\n\n===== Simplifying "+id)
  val ast = WebASTImporter.queryAndConvert(id)
  //println("=== ORIGINAL\n"+PrettyPrinter(ast))
  val sast = LoopRewriter(ast)
  //println("=== SIMPLIFIED\n"+PrettyPrinter(sast))
  Typer.processScript(sast)
  //println("=== SIMPLIFIED + TYPED\n"+PrettyPrinter(sast))
}

TestRunner(new NewScripts,100,compiler)