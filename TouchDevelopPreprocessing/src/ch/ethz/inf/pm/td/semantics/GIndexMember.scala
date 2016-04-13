/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{Modifier, ProgramPoint}
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.cloud.CloudUpdateWrapper
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.{Parameter, TypeName}
import RichNativeSemantics._

/**
 * @param typeName the name of the type
 */
case class GIndexMember(typeName: TypeName, keyFieldsParameters: List[Parameter], valueFieldsParameters: List[Parameter], modifiers:Set[Modifier]) extends AAny {

  lazy val keyFields:Set[ApiField] = TypeList.toTouchFields(keyFieldsParameters)
  lazy val valueFields:Set[ApiField] = TypeList.toTouchFields(valueFieldsParameters)

  def member_clear_fields = ApiMember(
    name = "clear fields",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated = true),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        var curState = state
        for (valueField <- valueFields) {
          curState = AssignField[S](this0, valueField, Invalid(valueField.typ, "fields may have been cleared"))(curState, pp)
        }
        curState
      }
    },modifiers)
  )

  def member_is_deleted = ApiMember(
    name = "is deleted",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated = true),
    returnType = TBoolean,
    semantics = ValidPureSemantics
  )

  lazy val member_confirmed = ApiMember(
    name = "confirmed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = ValidPureSemantics
  )

  override def possibleFields =
    super.possibleFields ++ keyFields ++ valueFields

  override lazy val declarations =
    super.declarations ++ mkGetterSetters(valueFields ++ keyFields) +
      (
        "clear fields" -> member_clear_fields,
        "is deleted" -> member_is_deleted,
        "confirmed" -> member_confirmed
        )

}
