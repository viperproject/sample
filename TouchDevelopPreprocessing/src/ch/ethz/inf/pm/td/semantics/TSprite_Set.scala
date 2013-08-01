package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

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

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Add sprite to set. Returns true if sprite was not already in set. */
    case "add" =>
      val List(sprite) = parameters // Sprite
      val state1 = super.forwardSemantics(this0,method,parameters,returnedType)
      Top[S](TBoolean.typ)(state1,pp)

    /** Add sprite to set and remove from old set. Returns true if sprite was in old set and not in new set. */
    case "add from" =>
      val List(old_set,sprite) = parameters // Sprite_Set,Sprite
      var curState = state
      curState = CallApi[S](this0,"add",List(sprite),TNothing.typ)(curState,pp)
      val resultA = curState.getExpression()
      curState = CallApi[S](old_set,"remove",List(sprite),TBoolean.typ)(curState,pp)
      val resultB = curState.getExpression()
      Return[S](resultA && resultB)

    case "index of" =>
      val List(item) = parameters
      Return[S](0 ndTo CollectionSize[S](this0))

    /** Remove sprite that was added to set first. */
    case "remove first" =>
      Error[S](CollectionSize[S](this0) < 1, "remove first", "Remove first is called on a possibly empty set")
      val x = CollectionAt[S](this0,toRichExpression(0))
      Return[S](x)(CollectionRemove[S](this0,toRichExpression(0)),pp)


    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}