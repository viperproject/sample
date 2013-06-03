import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.webapi.DowncasedTypeHints
import ch.ethz.inf.pm.td.webapi.ScriptRecord
import ch.ethz.inf.pm.td.webapi.URLFetcher._
import ch.ethz.inf.pm.td.webapi._
import java.io.{File, PrintWriter}
import net.liftweb.json._

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
    val p = new PrintWriter(new File(dir+"/"+id+".json"))
    try {
      p.println(fetchFile(ScriptListings.webastURLfromPubID(id)))
    } finally { p.close() }
  } else { println("could not create dir") }

})
