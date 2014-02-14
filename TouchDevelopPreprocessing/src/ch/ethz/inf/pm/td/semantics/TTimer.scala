
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Timer
 *
 * A timer
 *
 * @author Lucas Brutschy
 */

object TTimer {

  /** is the timer active */
  val field_is_active = new TouchField("is active", TBoolean.typName)

  /** is this an interval timer that fires regularly */
  val field_is_interval = new TouchField("is interval", TBoolean.typName)

  /** PRIVATE HANDLER FIELDS */
  val field_trigger_handler = new TouchField("trigger handler", TAction.typName)

  val typName = "Timer"
  val typ = new TouchType(typName, fields = List(field_is_active, field_is_interval, field_trigger_handler))

}

class TTimer extends AAny {

  def getTyp = TTimer.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {


    /** Clears the handlers and pauses the timer */
    case "clear" =>
      Skip

    /** sets the action to perform when the timer fires */
    case "on trigger" =>
      val List(perform) = parameters // Action
    val newState = AssignField[S](this0, TTimer.field_trigger_handler, perform)
      New[S](TEvent_Binding.typ)(newState, pp)

    /** deactivates the timer */
    case "pause" =>
      AssignField[S](this0, TTimer.field_is_active, False)

    /** reactives the timer */
    case "resume" =>
      AssignField[S](this0, TTimer.field_is_active, True or False)

    /** set the regular interval in seconds at which this timer fires */
    case "set interval" =>
      val List(seconds) = parameters // Number
      AssignField[S](this0, TTimer.field_is_interval, True)

    /** set the time in seconds after which this timer fires once */
    case "set timeout" =>
      val List(seconds) = parameters // Number
      AssignField[S](this0, TTimer.field_is_interval, False)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
