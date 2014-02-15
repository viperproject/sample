
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Map Pushpin
 *
 * A map pushpin
 *
 * @author Lucas Brutschy
 */

object TMap_Pushpin {

  /** Gets the pushpin geo location */
  val field_location = new TouchField("location", TLocation.typName)

  /** Shows or hides the pushpin */
  val field_visible = new TouchField("visible", TBoolean.typName)

  /** PRIVATE HANDLER FIELDS */
  val field_tap_handler = new TouchField("tap handler", TPosition_Action.typName)

  val typName = "Map Pushpin"
  val typ = new TouchType(typName, fields = List(field_location, field_visible, field_tap_handler))

}

class TMap_Pushpin extends AAny {

  def getTyp = TMap_Pushpin.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Set the handler invoked when the pushpin is tapped */
    case "on tap" =>
      val List(tapped) = parameters // Action
    val newState = AssignField[S](this0, TMap_Pushpin.field_tap_handler, tapped)
      New[S](TEvent_Binding.typ)(newState, pp)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
