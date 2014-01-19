import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi._
import java.text.SimpleDateFormat

val toAnalyze =
  """
  """.stripMargin.split("[\n,]").map(_.trim)

val skipList =
  """
  """.stripMargin.split("[\n,]").map(_.trim).toSet

def countLines(id:String):Int = {
  val x = URLFetcher.fetchFile(ScriptListings.codeURLfromPubID(id))
  x.split("\n").length
}

var i = 0

def analyzer(id:String) {
  if (!id.isEmpty && !skipList.contains(id)) {
    i = i + 1
    println(id+"\t"+i+"\t"+countLines(id))
    TestRunner.runIdWithApron(id)
  }
}

for(a <- toAnalyze) {
  analyzer(a)
}

TestRunner(new NonErroneousFeaturedScriptsBefore(new SimpleDateFormat("dd/MM/yyyy").parse("22/05/2013")),10000,analyzer)

