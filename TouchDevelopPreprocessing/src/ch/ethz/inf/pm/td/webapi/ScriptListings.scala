package ch.ethz.inf.pm.td.webapi

import net.liftweb.json._
import ch.ethz.inf.pm.td.webapi.URLFetcher._
import ch.ethz.inf.pm.td.compiler.TouchException
import ch.ethz.inf.pm.sample.oorepresentation.IteratorOverPrograms

/**
 * Fetches real scripts from the TouchDevelop website for testing purposes
 *
 * Lucas Brutschy
 * Date: 05.07.12
 * Time: 12:56
 */

case class ScriptRecord (
  time: Int,
  id: String,
  url: String,
  name: String,
  description: String,
  userid: String,
  username: String,
  //userscore: Int,
  //userhaspicture: Boolean,
  //icon: String,
  //iconbackground: String,
  //iconurl: String,
  //positivereviews: Int,
  //subscribers: Int,
  comments: Int,
  screenshots: Int,
  //capabilities: List[String],
  //flows: List[String],
  haserrors: Boolean,
  rootid: String,
  updateid: String,
  ishidden: Boolean,
  islibrary: Boolean,
  installations: Int,
  runs: Int,
  screenshotthumburl: String,
  screenshoturl: String) {

  def getAstURL:String = ScriptListings.astURLfromPubID(id)
  def getCodeURL:String = ScriptListings.codeURLfromPubID(id)

}

object ScriptListings {

  val baseURL = "http://www.touchdevelop.com/api/"
  val options = "" // "?original=true"
  val text = "/text"
  val ast = "/ast"
  val webast = "/webast"

  def webastURLfromPubID(pub:String):String = baseURL+pub+webast+options
  def astURLfromPubID(pub:String):String = baseURL+pub+ast+options
  def codeURLfromPubID(pub:String):String = baseURL+pub+text+options

  def pubIDfromFilename(fileName:String):String = {
    """([^/\\.]*)[.]([^/\\.]*)$""".r.findFirstMatchIn(fileName) match {
      case Some(matc) => matc.group(1)
      case _ => throw new TouchException("Filename regular expression did not match anything")
    }
  }

  def pubIDfromURL(url:String):String = {
    (baseURL+"""([^/]*)"""+text+"""\??.*""").r.findFirstMatchIn(url) match {
      case Some(matc) => matc.group(1)
      case _ => throw new TouchException("URL regular expression did not match anything")
    }
  }


}

abstract class ScriptListings extends IteratorOverPrograms {

  protected val service = "scripts?"

  private var continuation:String = null
  private var hasMore = true
  private var scripts: List[ScriptRecord] = Nil

  implicit val formats = new DefaultFormats {
    override val typeHintFieldName = "type"
    override val typeHints = DowncasedTypeHints(List(classOf[ScriptRecord]))
  }

  def reset() {
    continuation = null
    hasMore = true
    scripts = Nil
  }

  def get():ScriptRecord = {
    scripts match {
      case head :: tail => { scripts = tail; head }
      case Nil => if (hasMore) {prepareMore(); get()} else throw new NoMoreScriptsException
    }
  }

  override def hasNext()=hasMore;

  /**
   * Due to the way this is implemented, this may return the empty string
   * @return
   */
  override def next() : String = {
    val x = this.get()
    if (!x.haserrors) this.get().getCodeURL
    else ""

  def prepareMore() {
    scripts = scripts ::: getNextScripts
  }

  private def getNextScripts: List[ScriptRecord] = {

    val url = if (continuation != null) ScriptListings.baseURL + service + "continuation=" + continuation else ScriptListings.baseURL + service
    val json = parse(fetchFile(url))

    continuation = (json \ "continuation").extract[String]
    hasMore = continuation != null

    filter(for {
      JObject(root) <- json
      JField("items", JArray(items)) <- root
      item <- items
    } yield (item.extract[ScriptRecord]))

  }

  protected def filter(s : List[ScriptRecord]) : List[ScriptRecord]= s

}

class TopScripts extends ScriptListings {
  override protected val service = "top-scripts?"

  override def getLabel() = "TouchDevelop top scripts"
}

class NewScripts extends ScriptListings {
  override protected val service = "new-scripts?"

  override def getLabel() = "TouchDevelop new scripts"
}

class FeaturedScripts extends ScriptListings {
  override protected val service = "featured-scripts?"

  override def getLabel() = "TouchDevelop featured scripts"
}

class RootScripts() extends ScriptListings {
  override protected def filter(s : List[ScriptRecord]) : List[ScriptRecord]=
    s.filter( (t : ScriptRecord) => t.id.equals(t.rootid))

  override def getLabel() = "TouchDevelop root scripts"
}


class ScriptSearch(query:String) extends ScriptListings {
  override protected val service = "search?q="+query+"&"

  override def getLabel() = "TouchDevelop search scripts, query: "+query
}

class SampleScript extends ScriptListings {
  override protected val service = "pboj/scripts?"

  override def getLabel() = "TouchDevelop sample scripts"
}

class RootSampleScripts extends ScriptListings {
  override protected val service = "pboj/scripts?"

  override def getLabel() = "TouchDevelop sample scripts"

  override protected def filter(s : List[ScriptRecord]) : List[ScriptRecord]=
    s.filter( (t : ScriptRecord) => t.id.equals(t.rootid))
}

class ScriptsBefore(d:java.util.Date) extends ScriptListings {

  override protected val service = "scripts?"

  override def getLabel() = "TouchDevelop scripts before a given date"

  override protected def filter(s : List[ScriptRecord]) : List[ScriptRecord]=
    s.filter( { t : ScriptRecord =>
      val dT = new java.util.Date(t.time.asInstanceOf[Long]*1000)
      dT.before(d)
    })
}

class RootScriptsSearch(query : String) extends ScriptListings {

  override def getLabel() = "TouchDevelop root search scripts, query: "+query

  override protected val service = "search?q="+query+"&"

  override protected def filter(s : List[ScriptRecord]) : List[ScriptRecord]=
    s.filter( (t : ScriptRecord) => t.id.equals(t.rootid))
}

class NoMoreScriptsException extends Exception

/** When reading the class name from the json type hint field, convert first char to upper case */
case class DowncasedTypeHints(hints: List[Class[_]]) extends TypeHints {
  def hintFor(msgClass: Class[_]): String = {
    val shortNameIdx = msgClass.getName.lastIndexOf(".") + 1
    msgClass.getName.substring(shortNameIdx, shortNameIdx + 1).toLowerCase +
      msgClass.getName.substring(shortNameIdx + 1)
  }

  def classFor(hint: String) = hints find (hintFor(_) == hint)
}