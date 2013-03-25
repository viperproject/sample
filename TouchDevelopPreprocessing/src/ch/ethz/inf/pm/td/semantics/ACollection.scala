package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchCollection
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalAnalysisConstants

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
      CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
      Return[S](CollectionAt[S](this0,index))

    /** Gets the i-th element */
    case "at_index" =>
      val List(index) = parameters // Key_Type
      CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
      Return[S](CollectionAt[S](this0,index))

    /** Creates a copy of the given collection. AUXILIARY FUNCTION FOR FOREACH LOOPS */
    case "copy" =>
      CollectionCopy[S](this0)

    /** Get random element */
    case "random" =>
      If[S]( CollectionSize[S](this0) equal 0, Then = {
        Return[S](Invalid(this0.getType().asInstanceOf[TouchCollection].getValueType))(_,pp)
      }, Else = {
        Return[S](CollectionSummary[S](this0))(_,pp)
      })

    /** Get random element */
    case "rand" =>
      If[S]( CollectionSize[S](this0) equal 0, Then = {
        Return[S](Invalid(this0.getType()))(_,pp)
      }, Else = {
        Return[S](CollectionSummary[S](this0))(_,pp)
      })

    /** Returns the length of the collection*/
    case "count" =>
      Return[S](CollectionSize[S](this0))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
