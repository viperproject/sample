package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * Specifies the abstract semantics of bazaar
 *
 * Browse and review scripts from the bazaar
 *
 * @author Lucas Brutschy
 */

object SBazaar {

  /** Gets the current score for the current script */
  val field_leaderboard_score = new TouchField("leaderboard_score",TNumber.typ)

  val typName = "bazaar"
  val typ = new TouchType(typName, isSingleton = true, List(field_leaderboard_score))

}

class SBazaar extends AAny {

  def getTyp = SBazaar.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Posts the current game score to the script leaderboard */
    case "post_leaderboard_score" =>
      val List(score) = parameters // Number
      Assign(Field[S](this0,SBazaar.field_leaderboard_score),score)

    /** Posts the current game leaderboard to the wall */
    case "post_leaderboard_to_wall" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
