
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TNumber_Map
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Number Map
 *
 * A map of numbers to numbers
 *
 * TODO: These implementations are only valid for (size,elem) abstractions
 *
 * @author Lucas Brutschy
 */

object TNumber_Map extends Default_TNumber_Map {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {


    /** === NUMBER MAPS RETURN 0 FOR UNITIALIZED FIELDS! === */
    case "at" =>
      val List(key) = parameters // Key_Type
      If[S](collectionContainsKey[S](this0, key), Then={
        Return[S](collectionAt[S](this0, key))(_, pp)
      }, Else={
        Return[S](0)(_, pp)
      })

    /** Computes the average of the values */
    case "avg" =>
      Return[S](collectionAllValues[S](this0))

    /** Computes the maximum of the values */
    case "max" =>
      Return[S](collectionAllValues[S](this0))

    /** Computes the minimum of the values */
    case "min" =>
      Return[S](collectionAllValues[S](this0))

    /** Extracts the elements at indices between start (inclusive) and end (non-inclusive). */
    case "slice" =>
      val List(start,end) = parameters // Number,Number
      Clone[S](this0)

    /** Computes the sum of the values */
    case "sum" =>
      Return[S]( collectionSize[S](this0) * collectionAllValues[S](this0))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}

