
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.{Default_TPlace_Collection, Default_TPlace}
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Place Collection
 *
 * A collection of places
 *
 * @author Lucas Brutschy
 */ 

object TPlace_Collection extends Default_TPlace_Collection {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Sorts the places by distance to the location */
    case "sort by distance" =>
      val List(loc) = parameters // Location
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
