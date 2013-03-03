import ch.ethz.inf.pm.td.TestRunner
import ch.ethz.inf.pm.td.webapi.URLFetcher._
import ch.ethz.inf.pm.td.webapi.{Scripts, Script, DowncasedTypeHints}
import java.io.{File, PrintWriter}
import net.liftweb.json._

/**
 * Downloads all scripts of a specific user to a local directory.
 *
 * Set the userid below (e.g. pboj for the TouchDevelop Samples user)
 */

val dirs = "random1"

implicit val formats = new DefaultFormats {
  override val typeHintFieldName = "type"
  override val typeHints = DowncasedTypeHints(List(classOf[Script]))
}

TestRunner(new Scripts,10,{ (id:String,url:String) =>

  val dir = new File("TouchDevelopPreprocessing/testfiles/"+dirs)

  if (dir.isDirectory || dir.mkdir()) {
    val p = new PrintWriter(new File(dir+"/"+id+".td"))
    try {
      p.println(fetchFile(url))
    } finally { p.close() }
  } else { println("could not create dir") }

})

