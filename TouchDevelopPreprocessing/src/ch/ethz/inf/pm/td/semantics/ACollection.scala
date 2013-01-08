package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchCollection
import RichNativeSemantics._

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

    /** Get random element */
    case "random" =>
      Error[S](CollectionSize[S](this0) equal 0,"Calling random on a collection which might be empty!")
      New[S](getTyp.asInstanceOf[TouchCollection].getValueType)  // TODO: Get summary node

    /** Get random element */
    case "rand" =>
      Error[S](CollectionSize[S](this0) equal 0,"Calling rand on a collection which might be empty!")
      New[S](getTyp.asInstanceOf[TouchCollection].getValueType)  // TODO: Get summary node

    /** Returns the length of the collection*/
    case "count" =>
      Return[S](CollectionSize[S](this0))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
