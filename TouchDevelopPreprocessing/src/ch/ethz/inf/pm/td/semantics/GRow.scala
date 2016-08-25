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

case class GRow(typeName: TypeName, keyParameters:List[Parameter], fieldParameters:List[Parameter], modifiers:Set[Modifier]) extends AAny {

  lazy val fields:Set[ApiField] = TypeList.toTouchFields(keyParameters) ++ TypeList.toTouchFields(fieldParameters)

  override def possibleFields = super.possibleFields ++ (fields + GTable(this,modifiers).field_table)

  lazy val member_delete_row = ApiMember(
    name = "delete row",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated = true),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        CallApi[S](Field[S](this0, GTable(GRow(typeName,keyParameters,fieldParameters,modifiers),modifiers).field_table), "remove", List(this0), TBoolean)
      }
    },modifiers)
  )

  lazy val member_confirmed = ApiMember(
    name = "confirmed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = ValidPureSemantics
  )

  lazy val member_is_deleted = ApiMember(
    name = "is deleted",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = ValidPureSemantics
  )

  override lazy val declarations:Map[String,ApiMember] = super.declarations ++ Map(
      "delete row" -> member_delete_row,
      "is deleted" -> member_is_deleted,
      "confirmed" -> member_confirmed
    ) ++ mkGetterSetters(fields + GTable(this,modifiers).field_table)

}
