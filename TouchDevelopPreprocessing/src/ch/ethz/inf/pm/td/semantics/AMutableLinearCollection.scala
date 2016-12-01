/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, SemanticException, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichExpressionSet, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler._
import RichNativeSemantics._

/**
 * A mutable collection with integer indices
 */
trait AMutableLinearCollection extends ALinearCollection {

  override def isImmutable = false

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "add many" -> member_add_many,
    "add" -> member_add,
    "clear" -> member_clear,
    "contains" -> member_contains,
    "index of" -> member_index_of,
    "insert at" -> member_insert_at,
    "remove at" -> member_remove_at,
    "remove" -> member_remove,
    "reverse" -> member_reverse,
    "set at" -> member_set_at,
    "sort" -> member_sort
  )

  /** Never used: Adds many objects at once */
  def member_add_many = ApiMember(
    name = "add many",
    paramTypes = List(ApiParam(this)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds an object */
  def member_add = ApiMember(
    name = "add",
    paramTypes = List(ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Add[S](this0,parameters.head)
      }
    }
  )

  /** Never used: Removes all objects from the collection */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Clear[S](this0)
      }
    }
  )

  /** Never used: Checks if the item is in the collection */
  def member_contains = ApiMember(
    name = "contains",
    paramTypes = List(ApiParam(valueType)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(item) = parameters
        If[S](AllValues[S](this0) equal item, Then={
          Return[S](True)(_,pp)
        }, Else={
          Return[S](False)(_,pp)
        })
      }
    }
  )

  /** Never used: Gets the index of the first occurrence of an object. Returns -1 if not found or start is out of range. */
  def member_index_of = ApiMember(
    name = "index of",
    paramTypes = List(ApiParam(valueType), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(item,start) = parameters // Element_Type,Number

        if (start.typ != TNumber)
          throw new SemanticException("This is not a linear collection " + this0)

        If[S](IndexInRange[S](this0, start) && collectionContainsValue[S](this0, item) equal True , Then={
          Return[S](0 ndToIncl Count[S](this0)-1)(_, pp)
        }, Else={
          Return[S](-1)(_, pp)
        })
      }
    }
  )

  /** Never used: Inserts an object at position index. Does nothing if index is out of range. */
  def member_insert_at = ApiMember(
    name = "insert at",
    paramTypes = List(ApiParam(TNumber), ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {

        val List(index,item) = parameters // Number,Element_Type

        if (index.typ != TNumber)
          throw new SemanticException("This is not a linear collection " + this0)

        If[S](IndexInRange[S](this0, index), Then=(state) => {
          var newState = InvalidateKeys[S](this0)(state, pp)
          newState = Insert[S](this0, index, item)(newState, pp)
          IncreaseLength[S](this0)(newState, pp)
        }, Else=(state) => {
          state
        })

      }
    }
  )

  /** Never used: Removes the object at position index. */
  def member_remove_at = ApiMember(
    name = "remove at",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {

        val List(index) = parameters // Number

        if (index.typ != TNumber)
          throw new SemanticException("This is not a linear collection " + this0)

        If[S](IndexInRange[S](this0, index), Then=(state) => {
          var newState = RemoveAt[S](this0, index)(state, pp)
          newState = DecreaseLength[S](this0)(newState, pp)
          InvalidateKeys[S](this0)(newState, pp)
        }, Else={
          RemoveAt[S](this0, index)(_, pp)
        })

      }
    }
  )

  /** Never used: Removes the first occurence of an object. Returns true if removed. */
  def member_remove = ApiMember(
    name = "remove",
    paramTypes = List(ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TBoolean,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(item) = parameters // Element_Type

        //TODO
        //If[S](collectionContainsValue[S](this0, item) equal True, Then=(state) => {
        //  var newState = CollectionRemoveFirst[S](this0, item)(state, pp)
        //  newState = collectionDecreaseLength[S](this0)(newState, pp)
        //  newState = collectionInvalidateKeys(this0)(newState, pp)
        //  Return[S](True)(newState, pp)
        //}, Else= {
        //  Return[S](False)(_, pp)
        //})
        Return[S](False)(state, pp)
      }
    }
  )

  /** Never used: Reverses the order of objects in the collection */
  def member_reverse = ApiMember(
    name = "reverse",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        InvalidateKeys[S](this0)
      }
    }
  )

  /** Never used: Sets the object at position index. Does nothing if the index is out of range. */
  def member_set_at = ApiMember(
    name = "set at",
    paramTypes = List(ApiParam(TNumber), ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {

        val List(index, value) = parameters // Number,Element_Type

        if (index.typ != TNumber)
          throw new SemanticException("This is not a linear collection " + this0)

        If[S](IndexInRange[S](this0, index), Then=(state) => {
          //val newState = CollectionRemove[S](this0, index)(state, pp)
          //CollectionInsert[S](this0, index, value)(newState, pp)
          // FIXME: This is broken
          Insert[S](this0, index, value)(state,pp)
        }, Else=(state) => {
          state
        })

      }
    }
  )

  /** Never used: Sorts the strings in this collection */
  def member_sort = ApiMember(
    name = "sort",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        InvalidateKeys[S](this0)
      }
    }
  )

}