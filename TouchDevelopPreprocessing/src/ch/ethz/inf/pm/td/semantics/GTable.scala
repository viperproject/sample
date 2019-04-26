/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, Identifier, State}
import ch.ethz.inf.pm.sample.oorepresentation.{Modifier, ProgramPoint}
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.cloud.{CloudQuerySemantics, CloudQueryWrapper, CloudUpdateSemantics, CloudUpdateWrapper}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

case class GTable(rowTyp: GRow,modifiers:Set[Modifier]) extends AMutableLinearCloudCollection {

  override def typeName: TypeName = TypeName("Table",List(rowTyp.typeName))
  override def keyType = TNumber
  override def valueType = rowTyp

  /** Backlink. Not a field of this table, but a field of all rows of the table */
  val field_table = ApiField("*table", this)

  lazy val member_add_row = ApiMember(
    name = "add row",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated = true),
    returnType = rowTyp,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {

        // Create row with backlink to this table for removal
        var curState = state
        curState = New[S](rowTyp, initials = Map(field_table -> this0))(curState, pp)
        val row = curState.expr
        curState = Insert[S](this0, Count[S](this0)(curState, pp), row)(curState, pp)
        curState = IncreaseLength[S](this0)(curState, pp)
        Return[S](row)(curState, pp)

      }
    }, modifiers)
  )

  /** Just another name for at index */
  lazy val member_row_at = super.member_at_index

  lazy val member_invalid = ApiMember(
    name = "invalid",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = rowTyp,
    semantics = InvalidSemantics
  )

  lazy val member_invalid_row = ApiMember(
    name = "invalid row",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = rowTyp,
    semantics = InvalidSemantics
  )

  lazy val member_create_collection = ApiMember(
    name = "create collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(rowTyp),
    semantics = NewSemantics
  )

  lazy val member_entries_linked_to = ApiMember(
    name = "entries linked to",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(rowTyp),
    semantics = CloudQueryWrapper(DefaultSemantics, modifiers)
//      new ApiMemberSemantics {
//      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
//        val t = GCollection(rowTyp)
//
//        assert(valueType.keyField.isDefined)
//
//        val (mayMatching,mustMatching) = state.getFieldValueWhere(this0,field_entry.getField.get,field_entry.typ,
//          {
//            (x:Identifier,s:S) =>
//              !s.assume(Field[S](Field[S](x,entryType.field_value)(s,pp),valueType.keyField.get)(s,pp) equal parameters.head).isBottom
//          }
//        )
//
//        if (mayMatching.nonEmpty) True
//        else if (mustMatching.isEmpty) False
//        else True or False
//
//        New[S](t,initials = Map(
//          t.field_entry -> Field[S](this0,GTable.this.field_entry),
//          t.field_count -> Field[S](this0,GTable.this.field_count)
//        ))
//      }
//    }, modifiers)
  )


  lazy val member_copy_to_collection = ApiMember(
    name = "copy to collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(rowTyp),
    semantics = CloudQueryWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val t = GCollection(rowTyp)
        New[S](t,initials = Map(
          t.field_entry -> Field[S](this0,GTable.this.field_entry),
          t.field_count -> Field[S](this0,GTable.this.field_count)
        ))
      }
    }, modifiers)
  )

  override lazy val declarations:Map[String,ApiMember] = super.declarations ++
    Map(
      "add row" -> member_add_row,
      "invalid" -> member_invalid,
      "invalid row" -> member_invalid_row,
      "row at" -> member_row_at,
      "create collection" -> member_create_collection,
      "copy to collection" -> member_copy_to_collection,
      "entries linked to" -> member_entries_linked_to
    )


}
