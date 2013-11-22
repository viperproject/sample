import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi._
import java.text.SimpleDateFormat

val skipSet =
  """nyud
    |hqbd
    |gxhi
    |ildn
    |orug
    |tenl
    |oayea
    |cnyr
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
    TestRunner.runIdWithApron(id)
  }
}

//TestRunner("julr",analyzer _)
//TestRunner(new FeaturedScripts,10000,analyzer)
TestRunner(new NonErroneousFeaturedScriptsBefore(new SimpleDateFormat("dd/MM/yyyy").parse("09/04/2013")),100000,analyzer)
