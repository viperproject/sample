
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Map Pushpin
 *
 * A map pushpin
 *
 * @author Lucas Brutschy
 */

object TMap_Pushpin extends AAny {

  /** Gets the pushpin geo location */
  lazy val field_location = new ApiField("location", TLocation.typeName)

  /** Shows or hides the pushpin */
  lazy val field_visible = new ApiField("visible", TBoolean.typeName)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_tap_handler = new ApiField("tap handler", TPosition_Action.typeName)

  lazy val typeName = TypeName("Map Pushpin")

  override def possibleFields = super.possibleFields ++ List(field_location, field_visible, field_tap_handler)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Set the handler invoked when the pushpin is tapped */
    case "on tap" =>
      val List(tapped) = parameters // Action
    val newState = AssignField[S](this0, TMap_Pushpin.field_tap_handler, tapped)
      New[S](TEvent_Binding)(newState, pp)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
