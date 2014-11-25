
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{InvalidInitializer, ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Link
 *
 * A link to a video, image, email, phone number
 *
 * @author Lucas Brutschy
 */

object TLink extends AAny {

  /** Gets the url */
  lazy val field_address = new ApiField("address", TString.typeName)

  /** Gets the kind of asset - media, image, email, phone number, hyperlink, deep zoom link, radio */
  lazy val field_kind = new ApiField("kind", TString.typeName)

  /** Gets the location if any */
  lazy val field_location = new ApiField("location", TLocation.typeName, InvalidInitializer("link may not have a location"))

  /** Gets the name if any */
  lazy val field_name = new ApiField("name", TString.typeName, InvalidInitializer("link may not have a name"))

  lazy val typeName = TypeName("Link")

  override def possibleFields = super.possibleFields ++ List(field_address, field_kind, field_location, field_name)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Shares the link (email, sms, facebook, social or empty to pick from a list) */
    case "share" =>
      val List(network) = parameters // String
      // TODO: Check range of parameters
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
