import ch.ethz.inf.pm.td.parser.ScriptParser
import ch.ethz.inf.pm.td.symbols.Typer
import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.webapi.{Scripts, URLFetcher}

def typer(url:String) {
  println(url)
  val ast = ScriptParser(URLFetcher.fetchFile(url))
  Typer.processScript(ast)
}

TestRunner("https://www.touchdevelop.com/api/bohg/text",typer)
TestRunner(new Scripts,1000,typer)
