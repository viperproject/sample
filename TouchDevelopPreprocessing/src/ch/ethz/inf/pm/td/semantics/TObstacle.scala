
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Obstacle
 *
 * An obstacle on a board
 *
 * @author Lucas Brutschy
 */

object TObstacle {

  /** Color */
  val field_color = new TouchField("color", TColor.typName)

  /** Sets the obstacle thickness */
  val field_thickness = new TouchField("thickness", TNumber.typName)

  /** PRIVATE HANDLER FIELDS */
  val field_collision_handler = new TouchField("collision handler", TSprite_Action.typName)

  val typName = "Obstacle"
  val typ = DefaultTouchType(typName, fields = List(field_color, field_thickness, field_collision_handler))

}

class TObstacle extends AAny {

  def getTyp = TObstacle.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Delete the obstacle */
    case "delete" =>
      val List() = parameters // ignore
      Skip

    /** Attaches a handler where a sprite bounces on the obstacle */
    case "on collision" =>
      val List(bounce) = parameters // Sprite_Action
    val newState = AssignField[S](this0, TObstacle.field_collision_handler, bounce)
      New[S](TEvent_Binding.typ)(newState, pp)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
