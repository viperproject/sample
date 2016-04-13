/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.{ApiMemberSemantics, ApiParam, ApiMember, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TSprite_Set
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TSprite_Set extends Default_TSprite_Set {

  /** Never used: Adds an object */
  override def member_add = ApiMember(
    name = "add",
    paramTypes = List(ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TBoolean,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(sprite) = parameters // Sprite
        If[S](collectionContainsValue[S](this0, sprite) equal False, Then = (state) => {
          var newState = Insert[S](this0, Count[S](this0)(state, pp), sprite)(state, pp)
          newState = IncreaseLength(this0)(newState, pp)
          Return[S](True)(newState, pp)
        }, Else = {
          Return[S](False)(_, pp)
        })
      }
    }
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Add sprite to set and remove from old set. Returns true if sprite was in old set and not in new set. */
    case "add from" =>
      val List(old_set, sprite) = parameters // Sprite_Set,Sprite
    var curState = state
      curState = CallApi[S](this0, "add", List(sprite), TNothing)(curState, pp)
      val resultA = curState.expr
      curState = CallApi[S](old_set, "remove", List(sprite), TBoolean)(curState, pp)
      val resultB = curState.expr
      Return[S](resultA && resultB)(curState, pp)

    case "index of" =>
      val List(item) = parameters
      val ret = If[S](collectionContainsValue[S](this0, item) equal True, Then = {
        Return[S](0 ndToIncl Count[S](this0) - 1)(_, pp)
      }, Else = {
        Return[S](-1)(_, pp)
      })
      ret

    /** Remove sprite that was added to set first. */
    case "remove first" =>
      If[S](Count[S](this0) > 0, Then = (state) => {
        val result = At[S](this0, toRichExpression(0))(state,pp)
        var newState = RemoveAt[S](this0, toRichExpression(0))(state, pp)
        newState = DecreaseLength[S](this0)(newState, pp)
        newState = InvalidateKeys[S](this0)(newState, pp)
        Return[S](result)(newState, pp)
      }, Else = {
        Return[S](Invalid(valueType, "collection may be empty"))(_, pp)
      })

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}