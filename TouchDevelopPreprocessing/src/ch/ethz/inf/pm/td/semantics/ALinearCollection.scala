package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{SemanticException, ExpressionSet, State}
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalAnalysisConstants
import RichNativeSemantics._

/**
* This class represents collections that
* have linear integer keys.
**/
abstract class ALinearCollection extends ACollection {
  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S) = method match {
    case "at" =>
      val List(index) = parameters // Key_Type

      if (index.getType().getName() != TNumber.typName)
        throw new SemanticException("This is not a linear collection " + this0.toString)

      If[S](CollectionIndexInRange[S](this0, index), Then={
        Return[S](CollectionAt[S](this0, index))(_, pp)
      }, Else={
        Return[S](Invalid(this0.getType().asInstanceOf[TouchCollection].getValueType))(_, pp)
      })

    /** Gets the i-th element */
    case "at index" =>
      val List(index) = parameters // Key_Type
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
      Return[S](CollectionAt[S](this0,index))


    /** Get random element */
    case "random" =>
      If[S](CollectionSize[S](this0) > 0, Then={
        Return[S](CollectionSummary[S](this0))(_, pp)
      }, Else={
        Return[S](Invalid(this0.getType().asInstanceOf[TouchCollection].getValueType))(_, pp)
      })

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)
  }
}
