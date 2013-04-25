import ch.ethz.inf.pm.td.webapi.URLFetcher._
import ch.ethz.inf.pm.td.webapi.{ScriptRecord, DowncasedTypeHints}
import java.io.{File, PrintWriter}
import net.liftweb.json._

/**
 * Downloads all scripts of a specific user to a local directory.
 *
 * Set the userid below (e.g. pboj for the TouchDevelop Samples user)
 */

val userid = "pboj"

implicit val formats = new DefaultFormats {
  override val typeHintFieldName = "type"
  override val typeHints = DowncasedTypeHints(List(classOf[ScriptRecord]))
}

val json = parse(fetchFile("https://www.touchdevelop.com/api/"+userid+"/scripts"))

val scripts = for {
  JObject(root) <- json
  JField("items", JArray(items)) <- root
  item <- items
} yield (item.extract[ScriptRecord])

val dir = new File("TouchDevelopPreprocessing/testfiles/"+userid)

if (dir.isDirectory || dir.mkdir()) {

  for( s <- scripts ) {

    if ( !s.haserrors ) {

      val p = new PrintWriter(new File(dir+"/"+s.id+".td"))

      try {

        p.println(fetchFile(s.getCodeURL))

      } finally { p.close() }

    }

  }

} else { println("could not create dir") }

