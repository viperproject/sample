
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Collection
 *
 * A collection of objects
 *
 * @author Lucas Brutschy
 */

object GCollection {

  def typName(element: String) = element + " Collection"

  def typ(element: String) = new TouchCollection(typName(element), TNumber.typName, element)

}

class GCollection(element: String) extends AMutable_Collection {

  def getTyp = GCollection.typ(element)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {
    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
