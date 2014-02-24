
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Event Binding
 *
 * A handler attached to an event.
 *
 * @author Lucas Brutschy
 */

object TEvent_Binding {

  val typName = "Event Binding"
  val typ = DefaultTouchType(typName)

}

class TEvent_Binding extends AAny {

  def getTyp = TEvent_Binding.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Detaches the handler from the event. */
    case "delete" =>
      val List() = parameters // IGNORED
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
