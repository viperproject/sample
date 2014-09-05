package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/26/12
 * Time: 1:16 PM
 */
object TLocation_Collection extends AMutable_Collection {

  lazy val typeName = TypeName("Location Collection")

  def keyTypeName = TNumber.typeName
  def valueTypeName = TLocation.typeName

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Sorts by distance to the location */
    case "sort by distance" =>
      val List(loc) = parameters // Location
      Dummy[S](this0,method)
      Skip // Sorting is invariant for (size,elem) abstraction

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
