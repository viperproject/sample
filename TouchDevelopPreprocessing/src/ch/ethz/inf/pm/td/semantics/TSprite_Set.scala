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

  val typName = "Sprite_Set"
  val typ = TouchCollection(typName, "Number", "Sprite")

}

class TSprite_Set extends AMutable_Collection {

  def getTyp = TSprite_Set.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Add sprite to set. Returns true if sprite was not already in set. */
    case "add" =>
      val List(sprite) = parameters // Sprite
      super.forwardSemantics(this0,method,parameters)
      Return[S](True or False)

    /** Add sprite to set and remove from old set. Returns true if sprite was in old set and not in new set. */
    //case "add_from" =>
    //  val List(old_set,sprite) = parameters // Sprite_Set,Sprite
    //  New[S](TBoolean.typ) // TODO

    /** Remove sprite that was added to set first. */
    case "remove_first" =>
      Error[S](CollectionSize[S](this0) < 1, "Remove_first is called on a possibly empty set")
      CollectionRemove[S](this0,toRichExpression(0))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}