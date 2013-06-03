import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.parser.ScriptParser
import ch.ethz.inf.pm.td.webapi.{ScriptListings, URLFetcher}

/**
 *
 * Lucas Brutschy
 * Date: 8/13/12
 * Time: 3:36 PM
 *
 */
def prnt(id:String) {
  println(id)
  val url = ScriptListings.codeURLfromPubID(id)
  ScriptParser(URLFetcher.fetchFile(url))
}

println(ScriptParser("action main() { $a = \"//\"; }" ))
TestRunner("dnbf",prnt _) // THE COMMENT BUG
TestRunner(new ScriptListings,1000000,prnt _)
