package ch.ethz.inf.pm.td.webapi

import ch.ethz.inf.pm.sample.Reporter
import ch.ethz.inf.pm.td.compiler.TouchProgramPoint
import net.liftweb.json.{DefaultFormats, Serialization}

case class JResult (
    scriptID:String, // A public ID or a private ID (guid)
    items:List[JError]
  )

case class JError (
    nodeID:String, // the node at which the error occurs
    errorCategory:String, // a string to represent the error category (mostly for the feedback, when errors are ignored by the user). Could also be used to display different icons
    errorMessage:String, // a short explanation of the error
    slice:List[JAnnotation],
    fixes:List[JFix]
  )

case class JAnnotation (
    nodeID:String, // a node in the slice
    valueInformation:String, // the orange boxes in the pictures. The relevant values at this point in text format.
    description:String // the red boxes in the pictures. will be empty for many nodes
  )

case class JFix (
    name:String, // Name of the proposed fix
    replacements:List[JReplacement] // Node to be replaced
  )

case class JReplacement (
    nodeID:String,
    replacement:JNode // Textual representation of the replacement (without nodeIDs?)
  )

object ResultGenerator {

  def makeResult(scriptID:String):JResult = {

    val errors = (for ((string,pp) <- Reporter.seenErrors) yield {
      pp match {
        case TouchProgramPoint(scr,pos) =>
          if (scriptID == scr) {
            Some(JError(pos,"",string,Nil,Nil))
          } else None
        case _ => None
      }
    }).flatten.toList

    JResult(scriptID,errors)

  }

  def makeJson(scriptID:String):String = {
    toJson(makeResult(scriptID))
  }

  def toJson(result:JResult):String = {

    implicit val formats = new DefaultFormats {
      override val typeHintFieldName = "nodeType"
      override val typeHints = WebAstTypeHints(List(classOf[JResult],classOf[JError],classOf[JAnnotation],classOf[JFix],classOf[JReplacement])
      )
    }

    Serialization.write(result)

  }

  def printJson(scriptID:String) {
    println("---- BEGIN JSON RECORD")
    println(ResultGenerator.makeJson(scriptID))
    println("---- END JSON RECORD")
  }

}