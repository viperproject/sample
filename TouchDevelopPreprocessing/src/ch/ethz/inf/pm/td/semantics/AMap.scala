package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
import RichNativeSemantics._

/**
 * Represents a map collection in TouchDevelop
 */
trait AMap extends ACollection {

  /** Frequently used: Gets the value at a given key; invalid if not found */
  def member_at = ApiMember(
    name = "at",
    paramTypes = List(ApiParam(keyType)),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = DefaultSemantics
  )

  /** Rarely used: Removes the value at a given key */
  def member_remove = ApiMember(
    name = "remove",
    paramTypes = List(ApiParam(keyType)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the value at a given key; invalid if not found */
  def member_set_at = ApiMember(
    name = "set at",
    paramTypes = List(ApiParam(keyType), ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets many elements at once. */
  def member_set_many = ApiMember(
    name = "set many",
    paramTypes = List(ApiParam(this)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Clears the values from the map */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "at" -> member_at,
    "clear" -> member_clear,
    "is invalid" -> member_is_invalid,
    "post to wall" -> member_post_to_wall,
    "remove" -> member_remove,
    "set at" -> member_set_at,
    "set many" -> member_set_many
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    case "at" =>
      val List(key) = parameters // Key_Type

      val result = If[S](collectionContainsKey[S](this0, key) equal True, Then = {
        Return[S](collectionAt[S](this0, key))(_, pp)
      }, Else = {
        Return[S](Invalid(this0.getType().asInstanceOf[ACollection].valueType, "map may not contain the accessed key"))(_, pp)
      })

      result

    /** Gets the i-th key */
    case "at index" =>
      val List(index) = parameters
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive(index, 0, CollectionSize[S](this0) - NumericalAnalysisConstants.epsilon, "at index", "index")
      Return[S](collectionAllKeys[S](this0))

    case "set at" =>
      val List(key, value) = parameters // Number,Element_Type
      If[S](collectionContainsKey[S](this0, key) equal True, Then = (state) => {
        val s = collectionUpdate[S](this0, key, value)(state, pp)
        s
      }, Else = (state) => {
        val newState = collectionInsert[S](this0, key, value)(state, pp)
        val s = collectionIncreaseLength[S](this0)(newState, pp)
        s
      })

    /** Removes the element at the given key **/
    case "remove" =>
      val List(key) = parameters
      If[S](collectionContainsKey[S](this0, key) equal True, Then = (state) => {
        val newState = CollectionRemove[S](this0, key)(state, pp)
        collectionDecreaseLength[S](this0)(newState, pp)
      }, Else = {
        CollectionRemove[S](this0, key)(_, pp)
      })

    case "keys" =>
      CollectionExtractKeys[S](this0)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)
  }
}