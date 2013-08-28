import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi.{RootSampleScriptsBefore, RootSampleScripts}
import java.util.{GregorianCalendar, Date}


val skipSet = """""".stripMargin.split("[\n,]").map(_.trim).toSet

val skipUntil:Option[String] = None // Some("tktv") // None
var stillSkipping = true
var seenAlready:Set[String] = Set.empty
var i = 0
def analyzer(id:String) {
  if (seenAlready.contains(id)) { println("seenalready: "+id); return }
  seenAlready = seenAlready + id
  i = i + 1
  if (stillSkipping && skipUntil.isDefined) {
    if(skipUntil.get==id) stillSkipping = false
  } else {
    println(id+" "+i)
    if(!skipSet.contains(id)) {
     TestRunner.runIdWithApron(id)
    }
  }
}

TestRunner(new RootSampleScriptsBefore((new GregorianCalendar(2013,3,9)).getTime),10000,analyzer)









