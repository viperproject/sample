
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiMemberSemantics, ValidPureSemantics, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TBoard_Background_Scene
import ch.ethz.inf.pm.td.defsemantics.Default_TBoard_Background_Layer
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Board Background Scene
 *
 * A scene contains layers of parralax backgrounds.
 *
 * @author Lucas Brutschy
 */

object TBoard_Background_Scene extends Default_TBoard_Background_Scene {

  override lazy val member_create_layer = super.member_create_layer.copy(semantics = new ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S) = {
      var curState = state
      curState = Top[S](valueType)(curState,pp)
      val obj = curState.expr
      curState = Add[S](this0,obj)(curState,pp)
      curState = Return[S](obj)(curState,pp)
      curState
    }
  })

  override lazy val member_clear = super.member_clear.copy(semantics = new ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S) = {
      Clear[S](this0)
    }
  })

  /** Gets the view horizontal offset */
  lazy val field_view_x = ApiField("view x", TNumber)

  /** Gets the view vertical offset */
  lazy val field_view_y = ApiField("view y", TNumber)


  override def possibleFields = super.possibleFields ++ List(field_view_x, field_view_y)
}
      
