import java.io.{PrintWriter, File, FileOutputStream}
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

def argListToString(l:List[String]) = {
  l.map({x:String => x.replace(" ","_")}).mkString(",")
}

def argListToScala(l:List[String]) = {
  "List("+ (l map ("\"" + _ + "\"") mkString ",") + ")"
}

for ((thing,help,url) <- fetchMainInfo("https://www.touchdevelop.com/help/api")) {

  val fullURL = "https://www.touchdevelop.com"+ " ".r.replaceAllIn(url,"%20") // url encoding for stupids because the library is broken
  val subInfo = fetchSubInfo(fullURL)
  val className =
    if (thing.charAt(0).isUpper) "T"+thing.replace(" ","_")
    else "S"+thing.replaceFirst(thing.charAt(0).toString,thing.charAt(0).toUpper.toString).replace(" ","_")

  val p = new PrintWriter(new File("TouchDevelopPreprocessing/gen/"+className+".scala"))

  try {

    p.println(
      """
        |package ch.ethz.inf.pm.td.semantics
        |
        |import ch.ethz.inf.pm.td.compiler.TouchType
        |import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
        |import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
        |
        |/**
        | * Specifies the abstract semantics of XXX
        | *
        | * YYY
        | *
        | * @author Lucas Brutschy
        | */ """.stripMargin.replace("XXX",thing).replace("YYY",help)
    )


    if (thing.charAt(0).isUpper) {
      p.println(
        """
          |object TXXX {
          |
          |  val typName = "YYY"
          |  val typ = new TouchType(typName,isSingleton = false,List())
          |
          |}
          |
          |class TXXX extends AAny {
          |
          |  def getTyp = TXXX.typ
          |
          |  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
          |                                     (implicit pp:ProgramPoint,state:S):S = method match {
        """.stripMargin.replace("XXX",thing.replace(" ","_")).replace("YYY",thing))
    } else {
      p.println(
        """
          |object SXXX {
          |
          |  val typName = "YYY"
          |  val typ = new TouchType(typName,isSingleton = true,List())
          |
          |}
          |
          |class SXXX extends AAny {
          |
          |  def getTyp = SXXX.typ
          |
          |  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
          |                                     (implicit pp:ProgramPoint,state:S):S = method match {
        """.stripMargin.replace("XXX",thing.replaceFirst(thing.charAt(0).toString,thing.charAt(0).toUpper.toString).replace(" ","_")).replace("YYY",thing))
    }


    var fields:String = ""
    for ((property,signature,propHelp) <- subInfo) {
      if(property != "is invalid" && property != "post to wall") {
        val (args,returnVal) = splitLastColon(signature)
        p.println("    /** "+propHelp.trim+" */")
        p.println("    // case \""+property.replace(" ","_")+"\" => ")
        if (!args.isEmpty) {
          val argList = args.substring(1,args.length-1).split(",") map (splitLastColon(_)._1.replace(" ","_"))
          val typList = args.substring(1,args.length-1).split(",") map (splitLastColon(_)._2.replace(" ","_"))
          p.println("    //   val List("+argListToString(argList.toList)+") = parameters // "+argListToString(typList.toList))
        }
        if (returnVal != "Nothing") {
          p.println("    //   Return[S](Valid(T"+returnVal.replace(" ","_")+".typ))")
        } else {
          p.println("    //   Skip;")
        }

        // Is this just a getter for a field?
        if (args.isEmpty && returnVal != "Nothing") {
          p.println("    // DECLARATION AS FIELD: ")
          p.println("    //   /** "+propHelp.trim+" */")
          p.println("    //   val field_"+property.replace(" ","_")+" = new TouchField(\""+property.replace(" ","_")+"\",T"+returnVal.replace(" ","_")+".typ)")
          fields = fields + ", field_" + property.replace(" ","_")
        }

        p.println()
      }
    }

    p.println("    // FIELDS: "+fields+"\n")

    p.println(
      """    case _ =>
        |      super.forwardSemantics(this0,method,parameters)
        |
        |  }
        |}
      """.stripMargin)

  } finally { p.close() }

}


