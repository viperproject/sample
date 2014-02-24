
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Spring
 *
 * A spring between two sprites.
 *
 * @author Lucas Brutschy
 */

object TSpring {

  /** Sets the spring stiffness. */
  val field_stiffness = new TouchField("stiffness", TNumber.typName)

  val typName = "Spring"
  val typ = DefaultTouchType(typName, fields = List(field_stiffness))

}

class TSpring extends AAny {

  def getTyp = TSpring.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Deletes the spring */
    case "delete" =>
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
