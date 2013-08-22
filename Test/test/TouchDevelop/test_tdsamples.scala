import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi.{RootSampleScriptsBefore, RootSampleScripts}
import java.util.{GregorianCalendar, Date}


val skipSet = """""".stripMargin.split("[\n,]").map(_.trim).toSet

val skipUntil:Option[String] = None // Some("qvci") // None
var stillSkipping = true
var i = 0
def analyzer(id:String) {
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

TestRunner(new RootSampleScriptsBefore((new GregorianCalendar(2013,04,22)).getTime),10000,analyzer)









