package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._

/**
 * Specifies the abstract semantics of bazaar
 *
 * Browse and review scripts from the bazaar
 *
 * @author Lucas Brutschy
 */

object SBazaar {

  /** Gets the current score for the current script */
  val field_leaderboard_score = new TouchField("leaderboard score", TNumber.typName)

  /** Returns the user object of the current user */
  val field_current_user = new TouchField("current user", TUser.typName)

  val fields = List(field_leaderboard_score)
  val typName = "Bazaar"
  val typ = DefaultTouchType(typName, isSingleton = true, fields = List(field_current_user, field_leaderboard_score))

}

class SBazaar extends AAny {

  def getTyp = SBazaar.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {


    /** Returns the Abstract Syntax Tree JSON object for specified script */
    case "ast of" =>
      val List(id) = parameters // String
      TopWithInvalid[S](TJson_Object.typ, "script may not exist or infrastructure unreachable")

    /** [**obsolete**] Opens the leaderboard for the current script */
    case "open leaderboard" =>
      Error[S](Field[S](Singleton(SWeb.typ), SWeb.field_is_connected).not(), "open leaderboard",
        "Check if the device is connected to the internet before opening the leaderboard")
      Skip

    /** [**obsolete**] Opens the review page for the current script */
    case "open review" =>
      Error[S](Field[S](Singleton(SWeb.typ), SWeb.field_is_connected).not(), "open review",
        "Check if the device is connected to the internet before opening the review page")
      Skip

    /** [**obsolete**] Launches the bazaar. */
    case "open" =>
      Error[S](Field[S](Singleton(SWeb.typ), SWeb.field_is_connected).not(), "open",
        "Check if the device is connected to the internet before launching the bazaaar")
      Skip

    /** Asks the user to pick a script and return its identifier */
    case "pick script" =>
      val List(mode, message) = parameters // String,String
      TopWithInvalid[S](TString.typ, "use may abort script selection dialog")

    /** Posts the current game score to the script leaderboard */
    case "post leaderboard score" =>
      val List(score) = parameters // Number
      Assign(Field[S](this0, SBazaar.field_leaderboard_score), score)

    /** Posts the current game leaderboard to the wall */
    case "post leaderboard to wall" =>
      Skip

    /** Saves given Abstract Syntax Tree as a script */
    case "save ast" =>
      val List(id, script) = parameters // String,Json_Object
      Error[S](Field[S](Singleton(SWeb.typ), SWeb.field_is_connected).not(), "save ast",
        "Check if the device is connected to the internet")
      Skip

    /** Returns an identifier of either the top-level script or the current library */
    case "script id" =>
      val List(which) = parameters // String
      If[S]((which equal String("top")) or (which equal String("current")), { s =>
        Top[S](TString.typ)(s, pp)
      }, { s =>
        Return[S](Invalid(TString.typ, "argument to script id must be 'top' or 'current'"))(s, pp)
      })

    /** Returns a user object for a specified user id */
    case "user of" =>
      val List(id) = parameters // String
      TopWithInvalid[S](TUser.typ, "user id may not exist")

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
