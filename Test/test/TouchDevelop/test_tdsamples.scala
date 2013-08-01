import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi.{RootSampleScriptsBefore, RootSampleScripts}
import java.util.{GregorianCalendar, Date}


val skipSet =
  """uujx,llvn,vede,mbzn,maku,enoxa,yovpa,aapx,algi,tktv,hkqf,wfps,kvmi,eqcf,qvci,hbei,ileo,kyom
    |ecvs
    |gxkm
    |disx
    |qfsp
    |vrgt
    |hqxs
    |qurl
    |weqz
    |pyxj
    |yzfx,gdmr
    |zlup
    |djtr
    |zvpj
    |yzty
    |tglf,hrvg,frks
    |ywqu
  """.stripMargin.split("[\n,]").map(_.trim).toSet


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









