package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TSprite_Set {

  val typName = "Sprite Set"
  val typ = TouchCollection(typName, "Number", "Sprite")

}

class TSprite_Set extends AMutable_Collection {

  def getTyp = TSprite_Set.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Add sprite to set. Returns true if sprite was not already in set. */
    case "add" =>
      val List(sprite) = parameters // Sprite
      If[S](CollectionContainsValue[S](this0, sprite) equal False, Then = (state) => {
        var newState = CollectionInsert[S](this0, CollectionSize[S](this0)(state, pp), sprite)(state, pp)
        newState = CollectionIncreaseLength(this0)(newState, pp)
        Return[S](True)(newState, pp)
      }, Else = {
        Return[S](False)(_, pp)
      })

    /** Add sprite to set and remove from old set. Returns true if sprite was in old set and not in new set. */
    case "add from" =>
      val List(old_set, sprite) = parameters // Sprite_Set,Sprite
    var curState = state
      curState = CallApi[S](this0, "add", List(sprite), TNothing.typ)(curState, pp)
      val resultA = curState.expr
      curState = CallApi[S](old_set, "remove", List(sprite), TBoolean.typ)(curState, pp)
      val resultB = curState.expr
      Return[S](resultA && resultB)(curState, pp)

    case "index of" =>
      val List(item) = parameters
      If[S](CollectionContainsValue[S](this0, item) equal True, Then = {
        Return[S](0 ndTo CollectionSize[S](this0) - 1)(_, pp)
      }, Else = {
        Return[S](-1)(_, pp)
      })

    /** Remove sprite that was added to set first. */
    case "remove first" =>
      If[S](CollectionSize[S](this0) > 0, Then = (state) => {
        val result = state.getCollectionValue(CollectionAt[S](this0, toRichExpression(0))(state, pp)).expr
        var newState = CollectionRemove[S](this0, toRichExpression(0))(state, pp)
        newState = CollectionDecreaseLength[S](this0)(newState, pp)
        CollectionInvalidateKeys[S](this0)(newState, pp)
        Return[S](result)(newState, pp)
      }, Else = {
        Return[S](Invalid(this0.getType().asInstanceOf[TouchCollection].valueType, "collection may be empty"))(_, pp)
      })

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}