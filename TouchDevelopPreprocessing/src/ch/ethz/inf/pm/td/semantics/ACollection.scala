package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchCollection

/**
 * Represents a collection (this class contains common read operations. Extend AMutable_Collections to get write ops)
 */
abstract class ACollection extends AAny {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the i-th element */
    case "at" =>
      // FIXME: Some collections are indexed non-numerically
      val List(index) = parameters // Key_Type
      CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-1),method,"index")
      Return[S](CollectionAt[S](this0,index))

    /** Gets the i-th element */
    case "at_index" =>
      val List(index) = parameters // Key_Type
      CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-1),method,"index")
      Return[S](CollectionAt[S](this0,index))

    /** Creates a copy of the given collection. AUXILIARY FUNCTION FOR FOREACH LOOPS */
    case "copy" =>
      CollectionCopy[S](this0)

    /** Get random element */
    case "random" =>
      IfPossible[S]( CollectionSize[S](this0) equal 0, then = {
        IfPossible[S]( (CollectionSize[S](this0) equal 0).not(), then = {
          Return[S](CollectionSummary[S](this0),Invalid(this0.getType().asInstanceOf[TouchCollection].getValueType))
        }, els = {
          Return[S](Invalid(this0.getType().asInstanceOf[TouchCollection].getValueType))
        })
      }, els = {
        Return[S](CollectionSummary[S](this0))
      })

    /** Get random element */
    case "rand" =>
      IfPossible[S]( CollectionSize[S](this0) equal 0, then = {
        IfPossible[S]( (CollectionSize[S](this0) equal 0).not(), then = {
          Return[S](CollectionSummary[S](this0),Invalid(this0.getType()))
        }, els = {
          Return[S](Invalid(this0.getType()))
        })
      }, els = {
        Return[S](CollectionSummary[S](this0))
      })

    /** Returns the length of the collection*/
    case "count" =>
      Return[S](CollectionSize[S](this0))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
