import scala.None
import scala.Some
import org.xml.sax.InputSource

import scala.xml._
import parsing._


class HTML5Parser extends NoBindingFactoryAdapter {

  override def loadXML(source : InputSource, _p: SAXParser) = {
    loadXML(source)
  }

  def loadXML(source : InputSource) = {
    import nu.validator.htmlparser.{sax,common}
    import sax.HtmlParser
    import common.XmlViolationPolicy

    val reader = new HtmlParser
    reader.setXmlPolicy(XmlViolationPolicy.ALLOW)
    reader.setContentHandler(this)
    reader.parse(source)
    rootElem
  }
}

val html5parser = new HTML5Parser

def matchMainDiv(div:Node):Option[(String,String,String)] = {
  (div \ "@class").text match {
    case "api-toc-link" =>
      val thing = (div \ "a").text
      val help = (div \ "div").text
      val url = (div \ "a" \ "@href").text
      Some(thing,help,url)
    case _ => None
  }
}

def fetchMainInfo(url:String) = {
  val source = new org.xml.sax.InputSource(url)
  val xml = html5parser.loadXML(source)
  var res:List[(String,String,String)] = Nil
  for(div <- xml \\ "div") {
    matchMainDiv(div) match {
      case Some(x) =>
        res = res ::: List(x)
      case None => Unit
    }
  }
  res
}

def findHelp(div:Node):String = {
  for (subDiv <- div \ "div") {
    (subDiv \ "@class").text match {
      case "api-help" => return subDiv.text
      case _ => Unit
    }
  }
  throw new Exception("website changed")
}

def matchSubDiv(div:Node):Option[(String,String,String)] = {
  (div \ "@class").text match {
    case "api-prop" =>
      val property = (div \\ "a").text
      val returnVal = (div \\ "span").text
      val help = findHelp(div)
      Some(property,returnVal,help)
    case _ => None
  }
}

def fetchSubInfo(url:String) = {
  val source = new org.xml.sax.InputSource(url)
  val xml = html5parser.loadXML(source)
  var res:List[(String,String,String)] = Nil
  for(div <- xml \\ "div") {
    matchSubDiv(div) match {
      case Some(x) =>
        res = res ::: List(x)
      case None => Unit
    }
  }
  res
}

def splitLastColon(some:String) = {
  val col = some.lastIndexOf(":")
  (some.substring(0,col).trim,some.substring(col+1).trim)
}

def argListToScala(l:List[String]) = {
  "List("+ (l map ("\"" + _ + "\"") mkString ",") + ")"

}

println("  val api = API(List(")

for ((thing,help,url) <- fetchMainInfo("https://www.touchdevelop.com/help/api")) {

  val fullURL = "https://www.touchdevelop.com"+ " ".r.replaceAllIn(url,"%20") // url encoding for stupids because the library is broken
  val subInfo = fetchSubInfo(fullURL)

  if (thing.charAt(0).isUpper) print("    Type(\""+thing+"\", ")
  else print("    Singleton(\""+thing+"\", ")

  if (subInfo.length > 0) {
    println("List(\t// "+help.trim)
    for ((property,signature,propHelp) <- subInfo) {
      val (args,returnVal) = splitLastColon(signature)
      if (args.isEmpty) println("      Field(\""+property+"\",\""+returnVal+"\"),\t//"+propHelp.trim)
      else {
        val argList = args.substring(1,args.length-1).split(",") map (splitLastColon(_)._2)
        println("      Method(\""+property+"\","+argListToScala(argList.toList)+",\""+returnVal+"\"),\t//"+propHelp.trim)
      }
    }
    println("    )),")
  } else {
    println("Nil),\t// "+help.trim)
  }
}

println("  ))")



