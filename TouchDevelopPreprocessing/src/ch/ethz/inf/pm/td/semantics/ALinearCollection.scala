package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, SemanticException, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import RichNativeSemantics._

/**
 * This class represents collections that
 * have linear integer keys.
 **/
trait ALinearCollection extends ACollection {
  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S) = method match {
    case "at" =>
      val List(index) = parameters // Key_Type

      if (index.getType() != TNumber)
        throw new SemanticException("This is not a linear collection " + this0.toString)

      val newState = If[S](CollectionIndexInRange[S](this0, index), Then = {
        Return[S](CollectionAt[S](this0, index))(_, pp)
      }, Else = {
        Return[S](Invalid(this0.getType().asInstanceOf[ACollection].valueType, "collection access may be out of range"))(_, pp)
      })
      newState

    /** Gets the i-th element */
    case "at index" =>
      val List(index) = parameters // Key_Type
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
      Return[S](CollectionAt[S](this0, index))


    /** Get random element */
    case "random" =>
      If[S](CollectionSize[S](this0) > 0, Then = {
        Return[S](CollectionSummary[S](this0))(_, pp)
      }, Else = {
        Return[S](Invalid(this0.getType().asInstanceOf[ACollection].valueType, "collection may be empty"))(_, pp)
      })

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)
  }
}
