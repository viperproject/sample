
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Spring
 *
 * A spring between two sprites.
 *
 * @author Lucas Brutschy
 */

object TSpring extends AAny {

  /** Sets the spring stiffness. */
  lazy val field_stiffness = new ApiField("stiffness", TNumber.typeName)

  val typeName = TypeName("Spring")

  override def possibleFields = super.possibleFields ++ List(field_stiffness)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Deletes the spring */
    case "delete" =>
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
