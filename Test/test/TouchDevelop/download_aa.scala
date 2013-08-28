import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi.URLFetcher._
import ch.ethz.inf.pm.td.webapi._
import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat

/**
 * Downloads all scripts in JSON format
 */

val dirs = "icseAA5"

TestRunner(new NonErroneousAARootScriptsBefore(new SimpleDateFormat("dd/MM/yyyy").parse("10/06/2013")),100000000,{ id:String =>

  print(id+"...")

  if (id.startsWith("aa") || id.startsWith("ab")) {

    val dir = new File("Test/test/TouchDevelop/"+dirs)

    if (dir.isDirectory || dir.mkdir()) {
      val p = new PrintWriter(new File(dir+"/"+id+".json"))
      try {
        p.println(fetchFile(ScriptListings.webastURLfromPubID(id)))
        println("written")
      } finally { p.close() }
    } else { println("could not create dir") }

  } else {

    println("skipped")

  }

})



class NonErroneousAARootScriptsBefore(d:java.util.Date) extends ScriptListings {

  override protected val service = "scripts?count=100&"
  override def getLabel() = "AA,Root,NoError,Before"+new SimpleDateFormat("dd/MM/yyyy").format(d)

  override protected def filter(s : List[ScriptRecord]) : List[ScriptRecord]= {
    val res = s.filter( { t : ScriptRecord =>
      val dT = new java.util.Date(t.time.asInstanceOf[Long]*1000)
      dT.before(d) && t.id.equals(t.rootid) && !t.haserrors && (t.id.startsWith("aa") || t.id.startsWith("ab"))
    } )
    println("filtered "+(s.length - res.length)+" out of "+s.length)
    res
  }
}