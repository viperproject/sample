import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph
import ch.ethz.inf.pm.td.compiler.{TouchException, TouchCompiler}
import ch.ethz.inf.pm.td.parser.ScriptParser
import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.webapi.{NewScripts, URLFetcher, NoMoreScriptsException, FeaturedScripts}

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 3:59 PM
 *
 */

val comp = new TouchCompiler

def compiler(url:String) {
  println("== Compiling "+url)
  val cds = comp.compileFile(url)
  for(cd <- cds; m <- cd.methods) {
    ShowGraph.ShowControlFlowGraph(m.body)
  }
}

//TestRunner(new NewScripts,3,compiler)
//TestRunner("https://www.touchdevelop.com/api/qssc/text?original=true",compiler _)
TestRunner("https://www.touchdevelop.com/api/fdzm/text",compiler _)

while(true) ();
