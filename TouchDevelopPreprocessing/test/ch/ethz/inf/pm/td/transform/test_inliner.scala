import ch.ethz.inf.pm.td.parser.{PrettyPrinter, ScriptParser}
import ch.ethz.inf.pm.td.symbols.Typer
import ch.ethz.inf.pm.td.transform.Inliner
import ch.ethz.inf.pm.td.{webapi, TestRunner}
import ch.ethz.inf.pm.td.webapi.{NewScripts, URLFetcher}
import io.Source

/**
 *
 * Lucas Brutschy
 * Date: 9/20/12
 * Time: 9:43 AM
 *
 */

def inliner(url:String) {
  println("\n\n===== Simplifying "+url)
  val str = if (url.startsWith("http")) URLFetcher.fetchFile(url)
            else Source.fromFile(url).getLines().mkString("\n")
  val ast = ScriptParser(str)
  println("=== ORIGINAL\n"+PrettyPrinter(ast))
  val sast = Inliner.inline(ast)
  Typer.processScript(sast)
  println("=== INLINED\n"+PrettyPrinter(sast))
}

//TestRunner("TouchDevelopPreprocessing/testfiles/method_call.td", inliner _)
//TestRunner("https://www.touchdevelop.com/api/nuyf/text",inliner _)
TestRunner(new NewScripts,100,inliner _)
