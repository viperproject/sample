import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.typecheck.Typer
import ch.ethz.inf.pm.td.webapi.{WebASTImporter, Scripts}

def typer(id:String) {
  if( id != "byxva" && id != "yemc" && id != "tpll") {
    println(id)
    val script = WebASTImporter.queryAndConvert(id)
    Typer.processScript(script)
  }
}

//Typer.processScript(WebASTImporter.queryAndConvert("mscv"))
//Typer.processScript(WebASTImporter.queryAndConvert("ehqe"))
//Typer.processScript(WebASTImporter.queryAndConvert("bkkimpmk"))
TestRunner(new Scripts,100000,typer _)


