/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.webapi

import java.text.SimpleDateFormat

import ch.ethz.inf.pm.td.compiler.TouchException
import ch.ethz.inf.pm.td.webapi.URLFetcher._
import net.liftweb.json._

/**
  * Record storing information about a script, as defined by the TouchDevelop API
  *
  * DO NOT MODIFY THE NAMES OF THE FIELD OF THIS CLASS,
  * AS IT IS CONSTRUCTED BY DESERIALIZATION OF A JSON OBJECT
  */
case class ScriptRecord(
    time: Int,
    id: String,
    name: String,
    description: String,
    userid: String,
    username: String,
    comments: Int,
    screenshots: Int,
    rootid: String,
    baseid: String,
    updateid: String,
    ishidden: Boolean,
    islibrary: Boolean,
    toptagids: List[String]) {

  def getAstURL: String = ScriptQuery.astURLfromPubID(id)

  def getCodeURL: String = ScriptQuery.codeURLfromPubID(id)

}

object ScriptQuery {

  val baseURL = "https://www.touchdevelop.com/api/"
  val options = ""
  val text = "/text"
  val ast = "/ast"
  val webast = "/webast"

  def webastURLfromPubID(pub: String): String = baseURL + pub + webast + options

  def astURLfromPubID(pub: String): String = baseURL + pub + ast + options

  def codeURLfromPubID(pub: String): String = baseURL + pub + text

  def pubIDfromFilename(fileName: String): String = {
    """([^/\\.]*)[.]([^/\\.]*)$""".r.findFirstMatchIn(fileName) match {
      case Some(matc) => matc.group(1)
      case _ => throw TouchException("Filename regular expression did not match anything")
    }
  }

  def pubIDfromURL(url: String): String = {
    (baseURL + """([^/]*)""" + text + """\??.*""").r.findFirstMatchIn(url) match {
      case Some(matc) => matc.group(1)
      case _ => throw TouchException("URL regular expression did not match anything")
    }
  }

  def getScriptRecord(id: String): ScriptRecord = {

    implicit val formats = new DefaultFormats {
      override val typeHintFieldName = "type"
      override val typeHints = DowncasedTypeHints(List(classOf[ScriptRecord]))
    }

    val url = ScriptQuery.baseURL + id
    val content = fetchFile(url)
    parse(content).extract[ScriptRecord]
  }

}

class ScriptQuery extends IteratorOverPrograms {

  /**
    * Defines whether the given script record should be iterated or filtered out.
    * Make sure to call super.filter()
    *
    * @param s the given script record
    * @return true if s should be iterated
    */
  def filter(s: ScriptRecord): Boolean = true

  /**
    * The service of the web api to call. Can be overwritten by scripts, top-scripts, etc.
    *
    * @return
    */
  protected def service: String = "scripts"

  /**
    * A list of options that should be included in the query.
    * Make sure to define it as List(....)::super.searchOptions.
    */
  protected def searchOptions: List[String] = List("count=999")

  private var count: Int = 0
  private var limit: Option[Int] = None
  private var continuation: String = null
  private var hasMore = true
  private var scripts: List[ScriptRecord] = Nil

  implicit val formats = new DefaultFormats {
    override val typeHintFieldName = "type"
    override val typeHints = DowncasedTypeHints(List(classOf[ScriptRecord]))
  }

  def setLimit(a: Int) {
    limit = Some(a)
  }

  def reset() {
    continuation = null
    hasMore = true
    scripts = Nil
    count = 0
    limit = None
  }

  def get(): ScriptRecord = {
    if (limit.isDefined && limit.get <= count) throw new NoMoreScriptsException
    scripts match {
      case head :: tail => scripts = tail; count += 1; head
      case Nil => if (hasMore) {
        prepareMore()
        get()
      } else throw new NoMoreScriptsException
    }
  }

  override def hasNext() = hasMore && (!limit.isDefined || limit.get > count)

  override def next(): ScriptRecord = {
    this.get()
  }

  def prepareMore() {
    scripts = scripts ::: getNextScripts
  }

  private def getNextScripts: List[ScriptRecord] = {

    val opt = if (continuation != null) ("continuation=" + continuation) :: searchOptions else searchOptions
    val url = ScriptQuery.baseURL + service + "?" + opt.mkString("&")
    val content = fetchFile(url)
    val json = parse(content)

    continuation = (json \ "continuation").extract[String]
    hasMore = continuation != null

    (for (
      JObject(root) <- json;
      JField("items", JArray(items)) <- root;
      item <- items
    ) yield {
      item.extract[ScriptRecord]
    }).filter(filter _)

  }

  override def label = "scripts" + (limit match {
    case Some(x) => ",limit" + x;
    case None => ""
  })


}

trait HiddenFilter extends ScriptQuery {
  override def filter(s: ScriptRecord) = super.filter(s) && !s.ishidden

  override def label = super.label + ",no-hidden"
}

trait LibraryFilter extends ScriptQuery {
  override def filter(s: ScriptRecord) = super.filter(s) && !s.islibrary

  override def label = super.label + ",no-libs"
}

trait RootFilter extends ScriptQuery {
  override def filter(s: ScriptRecord) = super.filter(s) && s.id.equals(s.rootid)

  override def label = super.label + ",root"
}

trait DateFilter extends ScriptQuery {

  private var d: java.util.Date = new java.util.Date()

  // now
  def setDate(s: java.util.Date) {
    d = s
  }

  def setDate(s: String) {
    d = new SimpleDateFormat("dd/MM/yyyy").parse(s)
  }

  def setDate(y: Int, m: Int, da: Int) {
    d = new SimpleDateFormat("dd/MM/yyyy").parse(da + "/" + m + "/" + y)
  }

  override def filter(s: ScriptRecord) = {
    val dT = new java.util.Date(s.time.asInstanceOf[Long] * 1000)
    super.filter(s) && dT.before(d)
  }

  override def label = super.label + ",before" + new SimpleDateFormat("dd/MM/yyyy").format(d)

}

trait PrefixFilter extends ScriptQuery {

  private var prefix: String = ""

  // any
  def setPrefix(s: String) {
    prefix = s
  }

  override def filter(s: ScriptRecord) = super.filter(s) && s.id.startsWith(prefix)

  override def label = super.label + ",prefix" + prefix

}

trait TopService extends ScriptQuery {
  override def service = "top-scripts"

  override def label = super.label + ",top-first"
}

trait NewService extends ScriptQuery {
  override def service = "new-scripts"

  override def label = super.label + ",new-first"
}

trait FeaturedService extends ScriptQuery {
  override def service = "featured-scripts"

  override def label = super.label + ",featured"
}

trait SampleService extends ScriptQuery {
  override protected val service = "pboj/scripts"

  override def label = super.label + ",sample"
}

trait SearchService extends ScriptQuery {

  private var query: String = ""

  // any
  def setQuery(s: String) {
    query = s
  }

  override def service = "search"

  override def searchOptions = ("q=" + query) :: super.searchOptions

  override def label = "TouchDevelop search scripts, query: " + query

}

class Scripts extends ScriptQuery

class NewScripts extends ScriptQuery with NewService

class TopScripts extends ScriptQuery with TopService

class FeaturedScripts extends ScriptQuery with FeaturedService

class ScriptSearch(query: String) extends ScriptQuery with SearchService {
  setQuery(query)
}

/**
  * A <code>IteratorOverPrograms</code> iterates over a list of paths from which programs can be retrieved
  *
  * @author Pietro Ferrara
  * @since 0.1
  */
trait IteratorOverPrograms extends Iterator[ScriptRecord] {

  /**
    * This method returns a short description of the iterator.
    * *
    *
    * @return a short the description of the iterator (e.g., files in a directory)
    */
  def label: String

}

class ReadIdsFromFile(file: String, val label: String) extends IteratorOverPrograms {

  val lines = scala.io.Source.fromFile(file).getLines()

  def hasNext: Boolean = lines.hasNext

  def next(): ScriptRecord = ScriptQuery.getScriptRecord(lines.next())

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