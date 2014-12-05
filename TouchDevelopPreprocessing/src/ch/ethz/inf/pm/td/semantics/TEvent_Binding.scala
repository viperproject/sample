
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TEvent_Binding
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Event Binding
 *
 * A handler attached to an event.
 *
 * @author Lucas Brutschy
 */

object TEvent_Binding extends Default_TEvent_Binding {

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
      
