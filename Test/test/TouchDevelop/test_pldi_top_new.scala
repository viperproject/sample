import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi._
import java.text.SimpleDateFormat

val toAnalyze =
  """eabq
    |htmh
    |aesx
    |chfi
    |jyaxa
    |rotnb
    |zpbb
    |lypy
    |nyud
    |ggdm
    |yxdfa
    |gtbd
    |hksc
    |ildn
    |sdcn
    |vhyw
    |vjxt
    |yvph
    |bflza
    |cmmw
    |fakx
    |gevqa
    |jbpv
    |tqyv
    |wldu
  """.stripMargin.split("[\n,]").map(_.trim)

def countLines(id:String):Int = {
  val x = URLFetcher.fetchFile(ScriptListings.codeURLfromPubID(id))
  x.split("\n").length
}

var i = 0

def analyzer(id:String) {
    i = i + 1
    println(id+"\t"+i+"\t"+countLines(id))
    TestRunner.runIdWithApron(id)
}

for(a <- toAnalyze) {
  analyzer(a)
}
