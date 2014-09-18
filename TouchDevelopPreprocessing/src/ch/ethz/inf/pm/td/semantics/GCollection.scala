
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichExpression
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Collection
 *
 * A collection of objects
 *
 * @author Lucas Brutschy
 */
case class GCollection(element: TypeName) extends AMutable_Collection {

  def typeName = TypeName(element + " Collection")
  def keyTypeName = TNumber.typeName
  def valueTypeName = element

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {
    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
