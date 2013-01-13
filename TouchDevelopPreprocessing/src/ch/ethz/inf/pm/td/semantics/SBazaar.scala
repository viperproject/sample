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

  val typName = "bazaar"
  val typ = TouchType(typName, isSingleton = true)

}

class SBazaar extends AAny {

  def getTyp = SBazaar.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the current score for the current script */
    case "leaderboard_score" =>
      Return[S](Environment.leaderboardScore) // Is always valid, even if no score was ever posted (then 0)

    /** Posts the current game score to the script leaderboard */
    case "post_leaderboard_score" =>
      val List(score) = parameters // Number
      Assign(Environment.leaderboardScore,score)

    /** Posts the current game leaderboard to the wall */
    case "post_leaderboard_to_wall" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
