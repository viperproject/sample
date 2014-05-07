
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Board Background Scene
 *
 * A scene contains layers of parralax backgrounds.
 *
 * @author Lucas Brutschy
 */

object TBoard_Background_Scene {

  /** Gets the view horizontal offset */
  val field_view_x = new TouchField("view x", TNumber.typName)

  /** Gets the view vertical offset */
  val field_view_y = new TouchField("view y", TNumber.typName)

  val typName = "Board Background Scene"
  val typ = TouchCollection(typName, "Number", "Board Background Layer", List(field_view_x, field_view_y))

}

class TBoard_Background_Scene extends ACollection {

  def getTyp = TBoard_Background_Scene.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Creates a new layer on the scene. The distance determines the order of rendering and how fast the layer moves */
    case "create layer" =>
      val List(distance, pic) = parameters // TODO: Number,Picture
      Top[S](TBoard_Background_Layer.typ)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
