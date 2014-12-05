
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TBoard_Background_Scene
import ch.ethz.inf.pm.td.defsemantics.Default_TBoard_Background_Layer
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Board Background Scene
 *
 * A scene contains layers of parralax backgrounds.
 *
 * @author Lucas Brutschy
 */

object TBoard_Background_Scene extends Default_TBoard_Background_Scene {

  /** Gets the view horizontal offset */
  lazy val field_view_x = ApiField("view x", TNumber)

  /** Gets the view vertical offset */
  lazy val field_view_y = ApiField("view y", TNumber)

  override def possibleFields = super.possibleFields ++ List(field_view_x, field_view_y)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Creates a new layer on the scene. The distance determines the order of rendering and how fast the layer moves */
    case "create layer" =>
      val List(distance, pic) = parameters // TODO: Number,Picture
      Top[S](TBoard_Background_Layer)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
