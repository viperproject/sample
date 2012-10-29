import ch.ethz.inf.pm.td.parser.{PrettyPrinter, ScriptParser}
import ch.ethz.inf.pm.td.symbols.Typer
import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.transform.{LoopRewriter, Linker, Inliner}
import ch.ethz.inf.pm.td.webapi.{Scripts, NewScripts, URLFetcher}
import io.Source

/**
 *
 * Lucas Brutschy
 * Date: 9/20/12
 * Time: 2:51 PM
 *
 */

def linker(url:String) {
  println("\n\n===== Linking "+url)
  val str = if (url.startsWith("http")) URLFetcher.fetchFile(url)
    else Source.fromFile(url).getLines().mkString("\n")

  val ast = ScriptParser(str)
  println("=== ORIGINAL\n"+PrettyPrinter(ast))
  Typer.processScript(ast)

  val linkedAST = Linker(ast,(x => ScriptParser(URLFetcher.fetchFile(Scripts.codeURLfromPubID(x)))))
  println("=== LINKED\n"+PrettyPrinter(linkedAST))
  Typer.processScript(linkedAST)

  val simplifiedAST = LoopRewriter(linkedAST)
  println("=== LOOPREWRITTEN\n"+PrettyPrinter(simplifiedAST))
  Typer.processScript(simplifiedAST)

  val inlinedAST = Inliner.inline(simplifiedAST)
  println("=== INLINED\n"+PrettyPrinter(inlinedAST))
  Typer.processScript(inlinedAST)
}

//TestRunner("https://www.touchdevelop.com/api/qssc/text",linker _) // Includes a huge number of libraries
//TestRunner("https://www.touchdevelop.com/api/xsmi/text",linker _) // Slightly simpler example
//TestRunner("https://www.touchdevelop.com/api/metz/text",linker _) // Recursive FAILS
//TestRunner("https://www.touchdevelop.com/api/sqdz/text",linker _) // Local call in foreach guard REQUIRES LOOP REWRITER
//TestRunner("https://www.touchdevelop.com/api/vjer/text",linker _) // multiple values FAILS
TestRunner("https://www.touchdevelop.com/api/oglu/text",linker _) // __diasemana_406 not found
TestRunner("https://www.touchdevelop.com/api/bkzw/text",linker _) // __s1_25 not found
//TestRunner(new NewScripts,100,linker _) // Random