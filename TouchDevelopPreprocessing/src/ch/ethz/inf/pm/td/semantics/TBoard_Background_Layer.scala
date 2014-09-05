
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Board Background Layer
 *
 * A background scene layer
 *
 * @author Lucas Brutschy
 */

object TBoard_Background_Layer extends AAny {

  lazy val typeName = TypeName("Board Background Layer")

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Gets a value indicating how the picture aligns horizontally. The default is `left`. */
    case "align x" =>
      val List() = parameters //
      Skip

    /** Gets a value indicating how the picture aligns vertically. The default is `top`. */
    case "align y" =>
      val List() = parameters //
      Skip

    /** Gets the layer distance */
    case "distance" =>
      val List() = parameters //
      Skip

    /** Gets the picture associated to the layer. */
    case "picture" =>
      val List() = parameters //
      Skip

    /** Gets a value indicating if the background repeats horizontally */
    case "repeat x" =>
      val List() = parameters //
      Skip

    /** Gets a value indicating if the background repeats horizontally */
    case "repeat y" =>
      val List() = parameters //
      Skip

    /** Sets a value indicating how the picture aligns horizontally. The default is `left`. */
    case "set align x" =>
      val List(align) = parameters // String
      Skip

    /** Sets a value indicating how the picture aligns vertically. The default is `top`. */
    case "set align y" =>
      val List(align) = parameters // String
      Skip

    /** Sets the layer distance */
    case "set distance" =>
      val List(d) = parameters // Number
      Skip

    /** Sets a value indicating if the background repeats horizontally */
    case "set repeat x" =>
      val List(repeat) = parameters // Boolean
      Skip

    /** Sets a value indicating if the background repeats horizontally */
    case "set repeat y" =>
      val List(repeat) = parameters // Boolean
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
