package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Represents
 */
abstract class ACollection extends AAny {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the i-th element */
    case "at" =>
      val List(index) = parameters // Key_Type
      CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-1),method,"index")
      Return[S](CollectionAt[S](this0,index))

    /** Returns the length of the collection*/
    case "count" =>
      Return[S](CollectionSize[S](this0))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
