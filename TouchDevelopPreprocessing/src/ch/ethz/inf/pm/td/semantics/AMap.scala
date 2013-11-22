package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Represents a map collection in TouchDevelop
 */
abstract class AMap extends ACollection {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    case "at" =>
      val List(key) = parameters // Key_Type

      val result = If[S](CollectionContainsKey[S](this0, key) equal  True, Then={
        Return[S](CollectionAt[S](this0, key))(_, pp)
      }, Else={
        Return[S](Invalid(this0.getType().asInstanceOf[TouchCollection].getValueType))(_, pp)
      })

      result

    /** Gets the i-th key */
    case "at index" =>
      val List(index) = parameters
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive(index, 0, CollectionSize[S](this0) - NumericalAnalysisConstants.epsilon, "at index", "index")
      Return[S](CollectionKeySummary[S](this0))

    case "set at" =>
      val List(key, value) = parameters // Number,Element_Type
      If[S](CollectionContainsKey[S](this0, key) equal True, Then=(state) => {
        val s = CollectionUpdate[S](this0, key, value)(state, pp)
        s
      }, Else=(state) => {
        val newState = CollectionInsert[S](this0, key, value)(state, pp)
        val s = CollectionIncreaseLength[S](this0)(newState, pp)
        s
      })

    case "set many" =>
      Skip //TODO: implement

    /** Removes the element at the given key **/
    case "remove" =>
      val List(key) = parameters
      If[S](CollectionContainsKey[S](this0, key) equal True, Then=(state) => {
        val newState = CollectionRemove[S](this0, key)(state, pp)
        CollectionDecreaseLength[S](this0)(newState, pp)
      }, Else={
        CollectionRemove[S](this0, key)(_, pp)
      })

    case "keys" =>
      CollectionExtractKeys[S](this0)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)
  }
}