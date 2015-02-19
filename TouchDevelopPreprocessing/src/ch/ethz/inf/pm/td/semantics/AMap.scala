package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{SemanticException, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichExpression, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._

/**
 * Represents a map collection in TouchDevelop
 */
trait AMap extends ACollection {

  lazy val keyCollectionTyp = this match {
    case TString_Map => GCollection(TString)
    case TNumber_Map => GCollection(TNumber)
    case TJson_Object => GCollection(TString)
    case TJson_Builder => GCollection(TString)
    case _ => throw new SemanticException("keys() operation is not supported for that object")
  }

  /**
   * This is imprecise, because we do not keep the relation between the collection
   * and its key collection
   */
  def collectionExtractKeys[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    var curState = state
    curState = New[S](keyCollectionTyp)(curState,pp)
    val keyCollection = curState.expr
    curState = keyCollectionTyp.collectionInsert(
      keyCollection,
      Valid(TNumber),
      this.collectionAllKeys[S](collection)(curState,pp)
    )(curState,pp)
    Return[S](keyCollection)(curState,pp)
  }

  override def member_at_index = super.member_at_index.copy(semantics = new ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      val List(index) = parameters
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive(index, 0, CollectionSize[S](this0) - NumericalAnalysisConstants.epsilon, "at index", "index")
      Return[S](collectionAllKeys[S](this0))
    }
  })

  /** Frequently used: Gets the value at a given key; invalid if not found */
  def member_at = ApiMember(
    name = "at",
    paramTypes = List(ApiParam(keyType)),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(key) = parameters // Key_Type

        val result = If[S](collectionContainsKey[S](this0, key) equal True, Then = {
          Return[S](collectionAt[S](this0, key))(_, pp)
        }, Else = {
          Return[S](Invalid(this0.getType().asInstanceOf[ACollection].valueType, "map may not contain the accessed key"))(_, pp)
        })

        result
      }
    }
  )

  /** Frequently used: Gets the value at a given key; invalid if not found */
  def member_keys = ApiMember(
    name = "keys",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = keyCollectionTyp,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S =
        collectionExtractKeys[S](this0)
    }
  )

  /** Rarely used: Removes the value at a given key */
  def member_remove = ApiMember(
    name = "remove",
    paramTypes = List(ApiParam(keyType)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(key) = parameters
        If[S](collectionContainsKey[S](this0, key) equal True, Then = (state) => {
          val newState = collectionRemoveAt[S](this0, key)(state, pp)
          collectionDecreaseLength[S](this0)(newState, pp)
        }, Else = {
          collectionRemoveAt[S](this0, key)(_, pp)
        })
      }
    }
  )

  /** Frequently used: Sets the value at a given key; invalid if not found */
  def member_set_at = ApiMember(
    name = "set at",
    paramTypes = List(ApiParam(keyType), ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(key, value) = parameters // Number,Element_Type
        If[S](collectionContainsKey[S](this0, key) equal True, Then = (state) => {
          val s = collectionUpdate[S](this0, key, value)(state, pp)
          s
        }, Else = (state) => {
          val newState = collectionInsert[S](this0, key, value)(state, pp)
          val s = collectionIncreaseLength[S](this0)(newState, pp)
          s
        })
      }
    }
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
    "keys" -> member_keys,
    "post to wall" -> member_post_to_wall,
    "remove" -> member_remove,
    "set at" -> member_set_at,
    "set many" -> member_set_many
  )
  
}