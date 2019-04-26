/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{Modifier, ProgramPoint}
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.cloud.{CloudQuerySemantics, CloudQueryWrapper, CloudUpdateWrapper}
import ch.ethz.inf.pm.td.compiler._

trait AIndex extends ACloudCollection {

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "clear" -> member_clear,
    "at" -> member_at,
    "invalid" -> member_invalid,
    "copy to collection" -> member_copy_to_collection
  )

  override def member_at_index = super.member_at_index.copy(
    semantics = new CloudQuerySemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        var curState = super.forwardSemantics[S](this0,method,parameters)
        curState = Return[S](AllValues[S](this0))(curState,pp)
        curState
      }
      override def typeModifiers: Set[Modifier] = modifiers
    })

  def member_copy_to_collection = ApiMember(
    name = "copy to collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(valueType),
    semantics = CloudQueryWrapper(ValidPureSemantics,modifiers)

  )

  def member_invalid = ApiMember(
    name = "invalid",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = InvalidSemantics
  )

  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Clear[S](this0)
      }
    },modifiers)
  )

  /** Sometimes used: Gets the picture at position 'index'; invalid if index is out of bounds */
  def member_at = ApiMember(
    name = "at",
    paramTypes = List(ApiParam(keyType)),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = CloudQueryWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val key = parameters.head
        If[S](ContainsKey[S](this0, key) equal False, Then=(state) => {
          var newState = New[S](valueType)(state,pp)
          val newIndexMember = newState.expr
          newState = Insert[S](this0, key, newIndexMember)(newState,pp)
          newState = IncreaseLength[S](this0)(newState, pp)
          Return[S](At[S](this0, key)(newState, pp))(newState, pp)
        }, Else=(state)=>{
          Return[S](At[S](this0, key)(state, pp))(state, pp)
        })
      }
    },modifiers)
  )

}
