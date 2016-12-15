/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleError}
import ch.ethz.inf.pm.td.compiler.{SpaceSavingProgramPoint, TouchCompiler, TouchProgramPointRegistry}
import ch.ethz.inf.pm.td.webapi.WebAST._
import ch.ethz.inf.pm.td.webapi.WebAstTypeHints
import net.liftweb.json.{DefaultFormats, Serialization}

case class JResult(
    scriptID: String, // A public ID or a private ID (guid)
    items: List[JError]
)

case class JError(
    nodeID: String, // the node at which the error occurs
    errorCategory: String, // a string to represent the error category (mostly for the feedback, when errors are ignored by the user). Could also be used to display different icons
    errorMessage: String, // a short explanation of the error
    slice: List[JAnnotation],
    fixes: List[JFix]
)

case class JAnnotation(
    nodeID: String, // a node in the slice
    valueInformation: String, // the orange boxes in the pictures. The relevant values at this point in text format.
    description: String // the red boxes in the pictures. will be empty for many nodes
)

case class JFix(
    name: String, // Name of the proposed fix
    replacements: List[JReplacement] // Node to be replaced
)

case class JReplacement(
    nodeID: String,
    replacement: JNode // Textual representation of the replacement (without nodeIDs?)
)

object JSONExporter extends FileSystemResultExporter {

  def getExtension = "json"

  def warningsToString(compiler: TouchCompiler, id: String): String = {
    makeJson(id)
  }

  private def makeJson(scriptID: String): String = {
    toJson(makeResult(scriptID))
  }

  private def makeResult(scriptID: String): JResult = {

    val errors = (for (SampleError(id, string, pp, causes) <- Reporter.assertionViolations) yield {
      pp match {
        case touchPP: SpaceSavingProgramPoint =>
          val tpp = TouchProgramPointRegistry.reg(touchPP.id)
          if (scriptID == tpp.scriptID) {
            Some(JError(tpp.fullPosString, "", string, Nil, Nil))
          } else None
        case _ => None
      }
    }).flatten.toList

    JResult(scriptID, errors)

  }

  private def toJson(result: JResult): String = {

    implicit val formats = new DefaultFormats {
      override val typeHintFieldName = "nodeType"
      override val typeHints = WebAstTypeHints(List(classOf[JResult], classOf[JError], classOf[JAnnotation], classOf[JFix], classOf[JReplacement])
      )
    }

    Serialization.write(result)

  }

}
