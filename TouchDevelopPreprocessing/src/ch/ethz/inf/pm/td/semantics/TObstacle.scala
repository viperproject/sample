
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Obstacle
 *
 * An obstacle on a board
 *
 * @author Lucas Brutschy
 */

object TObstacle extends AAny {

  /** Color */
  lazy val field_color = new ApiField("color", TColor.typeName)

  /** Sets the obstacle thickness */
  lazy val field_thickness = new ApiField("thickness", TNumber.typeName)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_collision_handler = new ApiField("collision handler", TSprite_Action.typeName)

  lazy val typeName = TypeName("Obstacle")

  override def possibleFields = super.possibleFields ++ List(field_color, field_thickness, field_collision_handler)

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
      New[S](TEvent_Binding)(newState, pp)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
