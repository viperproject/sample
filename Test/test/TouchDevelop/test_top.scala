import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.parser.PrettyPrinter
import ch.ethz.inf.pm.td.webapi._
import java.text.SimpleDateFormat
import java.util.GregorianCalendar

val skipSet =
  """ggdm
    |chfi
    |cnyr
    |yvph
  """.stripMargin.split("\n").map(_.trim).toSet

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 3:59 PM
 *
 */

var i = 0

// TODO
def countLines(id:String):Int = {

  val x = URLFetcher.fetchFile(ScriptListings.codeURLfromPubID(id))

  return x.split("\n").length

}


def analyzer(id:String) {
  if(!skipSet.contains(id)) {
    i = i + 1
    println(id+"\t"+i+"\t"+countLines(id))
    val japp = WebASTImporter.query(id)
    val convert = WebASTImporter.convert(japp)
    TestRunner.runIdWithApron(id)
  }
}

//TestRunner("julr",analyzer _)
//TestRunner(new TopScripts,10000,analyzer)
//TestRunner(new NonErroneousTopScriptsBefore(new SimpleDateFormat("dd/MM/yyyy").parse("01/07/2013")),10000,analyzer)
TestRunner(new RootTopScriptsBefore(new SimpleDateFormat("dd/MM/yyyy").parse("09/04/2013")),10000,analyzer)
//TestRunner(new NonErroneousTopScriptsBefore(new SimpleDateFormat("dd/MM/yyyy").parse("25/10/2013")),10000,analyzer)
