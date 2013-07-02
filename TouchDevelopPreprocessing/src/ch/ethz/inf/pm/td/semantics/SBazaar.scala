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
  val field_leaderboard_score = new TouchField("leaderboard score",TNumber.typName)

  val fields = List(field_leaderboard_score)
  val typName = "Bazaar"
  val typ = new TouchType(typName, isSingleton = true, fields = List(field_leaderboard_score))

}

class SBazaar extends AAny {

  def getTyp = SBazaar.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Posts the current game score to the script leaderboard */
    case "post leaderboard score" =>
      val List(score) = parameters // Number
      Assign(Field[S](this0,SBazaar.field_leaderboard_score),score)

    /** Posts the current game leaderboard to the wall */
    case "post leaderboard to wall" =>
      Skip

    /** Launches the bazaar. */
    case "open" =>
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"open",
        "Check if the device is connected to the internet before launching the bazaaar")
      Skip

    /** Opens the leaderboard */
    case "open leaderboard" =>
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"open leaderboard",
        "Check if the device is connected to the internet before opening the leaderboard")
      Skip

    /** Opens the review page for the current script */
    case "open review" =>
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"open review",
        "Check if the device is connected to the internet before opening the review page")
      Skip


    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
