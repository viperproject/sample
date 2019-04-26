/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{SemanticException, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichExpressionSet, RichNativeSemantics}
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

  override def member_at_index = super.member_at_index.copy(semantics = new ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      val List(index) = parameters
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive(index, 0, CollectionSize[S](this0) - NumericalAnalysisConstants.epsilon, "at index", "index")
      Return[S](AllKeys[S](this0))
    }
  })

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "at" -> member_at,
    "clear" -> member_clear,
    "is invalid" -> member_is_invalid,
    "keys" -> member_keys,
    "post to wall" -> member_post_to_wall,
    "remove" -> member_remove,
    "set at" -> member_set_at,
    "set many" -> member_set_many
  )

  /** Frequently used: Gets the value at a given key; invalid if not found */
  def member_at = ApiMember(
    name = "at",
    paramTypes = List(ApiParam(keyType)),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(key) = parameters // Key_Type

        val result = If[S](ContainsKey[S](this0, key) equal True, Then = {
          Return[S](At[S](this0, key))(_, pp)
        }, Else = {
          Return[S](Invalid(this0.typ.asInstanceOf[ACollection].valueType, "map may not contain the accessed key"))(_, pp)
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
        ExtractKeys[S](this0)
    }
  )

  /**
    * This is imprecise, because we do not keep the relation between the collection
    * and its key collection
    */
  def ExtractKeys[S <: State[S]](collection: RichExpressionSet)(implicit state: S, pp: ProgramPoint): S = {
    var curState = state
    curState = New[S](keyCollectionTyp)(curState, pp)
    val keyCollection = curState.expr
    curState = keyCollectionTyp.Insert(
      keyCollection,
      Valid(TNumber),
      this.AllKeys[S](collection)(curState, pp)
    )(curState, pp)
    curState = keyCollectionTyp.SetCount[S](
      keyCollection, Field[S](collection, field_count)(curState, pp)
    )(curState, pp)
    val res = Return[S](keyCollection)(curState, pp)
    res
  }

  /** Rarely used: Removes the value at a given key */
  def member_remove = ApiMember(
    name = "remove",
    paramTypes = List(ApiParam(keyType)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(key) = parameters
        If[S](ContainsKey[S](this0, key) equal True, Then = (state) => {
          val newState = RemoveAt[S](this0, key)(state, pp)
          DecreaseLength[S](this0)(newState, pp)
        }, Else = {
          RemoveAt[S](this0, key)(_, pp)
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
        If[S](ContainsKey[S](this0, key) equal True, Then = (state) => {
          val s = Update[S](this0, key, value)(state, pp)
          s
        }, Else = (state) => {
          val newState = Insert[S](this0, key, value)(state, pp)
          val s = IncreaseLength[S](this0)(newState, pp)
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
  
}