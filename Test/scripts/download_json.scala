import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi.URLFetcher._
import ch.ethz.inf.pm.td.webapi._
import java.io.{File, PrintWriter}

/**
 * Downloads all scripts in JSON format
 */

val dirs = "root"

implicit val formats = new DefaultFormats {
  override val typeHintFieldName = "type"
  override val typeHints = DowncasedTypeHints(List(classOf[ScriptRecord]))
}

TestRunner(new RootScripts,100000000,{ id:String =>

  val dir = new File("TouchDevelopPreprocessing/testfiles/"+dirs)

  if (dir.isDirectory || dir.mkdir()) {
    val p = new PrintWriter(new File(dir+ "/" +id+".json"))
    try {
      p.println(fetchFile(ScriptQuery.webastURLfromPubID(id)))
    } finally { p.close() }
  } else { println("could not create dir") }

})
