package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of bazaar
 *
 * Browse and review scripts from the bazaar
 *
 * @author Lucas Brutschy
 */

object SBazaar extends ASingleton {

  lazy val typeName = TypeName("Bazaar")

  /** Never used: Returns the Abstract Syntax Tree JSON object for specified script */
  lazy val member_ast_of = new ApiMember(
    "ast of",
    List(ApiParam(TString)),
    ApiParam(this),
    TJson_Object
  ) with DefaultSemantics

  /** Never used: Returns the user object of the current user */
  lazy val member_current_user = new ApiMember("current user", List(), ApiParam(this), TUser) with DefaultSemantics

  /** Sometimes used: Gets the current score for the current script */
  lazy val member_leaderboard_score = new ApiMember("leaderboard score", List(), ApiParam(this), TNumber) with DefaultSemantics

  /** Never used: [**dbg**] three-way merge script texts. Debug only: for testing. */
  lazy val member_merge3 = new ApiMember("merge3", List(ApiParam(TString), ApiParam(TString), ApiParam(TString)), ApiParam(this), TString) with DefaultSemantics

  /** Never used: [**obsolete**] Opens the leaderboard for the current script */
  lazy val member_open_leaderboard = new ApiMember("open leaderboard", List(), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: [**obsolete**] Opens the review page for the current script */
  lazy val member_open_review = new ApiMember("open review", List(), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: [**obsolete**] Launches the bazaar. */
  lazy val member_open = new ApiMember("open", List(), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: Asks the user to pick a script and return its identifier */
  lazy val member_pick_script = new ApiMember("pick script", List(ApiParam(TString), ApiParam(TString)), ApiParam(this), TString) with DefaultSemantics

  /** Frequently used: Posts the current game score to the script leaderboard */
  lazy val member_post_leaderboard_score = new ApiMember("post leaderboard score", List(ApiParam(TNumber)), ApiParam(this,isMutated=true), TNothing) with DefaultSemantics

  /** Sometimes used: Posts the current game leaderboard to the wall */
  lazy val member_post_leaderboard_to_wall = new ApiMember("post leaderboard to wall", List(), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: Saves given Abstract Syntax Tree as a script */
  lazy val member_save_ast = new ApiMember("save ast", List(ApiParam(TString), ApiParam(TJson_Object)), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: Returns an identifier of either the top-level script or the current library */
  lazy val member_script_id = new ApiMember("script id", List(ApiParam(TString)), ApiParam(this), TString) with CustomSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      val List(which) = parameters // String
      If[S]((which equal String("top")) or (which equal String("current")), { s =>
        Top[S](TString)(s, pp)
      }, { s =>
        Return[S](Invalid(TString, "argument to script id must be 'top' or 'current'"))(s, pp)
      })
    }
  }

  /** Never used: Returns a user object for a specified user id */
  lazy val member_user_of = new ApiMember("user of", List(ApiParam(TString)), ApiParam(this), TUser) with DefaultSemantics

  override lazy val declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "ast of" -> member_ast_of,
    "current user" -> member_current_user,
    "leaderboard score" -> member_leaderboard_score,
    "merge3" -> member_merge3,
    "open leaderboard" -> member_open_leaderboard,
    "open review" -> member_open_review,
    "open" -> member_open,
    "pick script" -> member_pick_script,
    "post leaderboard score" -> member_post_leaderboard_score,
    "post leaderboard to wall" -> member_post_leaderboard_to_wall,
    "save ast" -> member_save_ast,
    "script id" -> member_script_id,
    "user of" -> member_user_of
  )

  lazy val field_leaderboard_score = ApiField("leaderboard score",TNumber.typeName)
  lazy val field_current_user = ApiField("current user",TUser.typeName)

  override lazy val possibleFields = super.possibleFields ++ Set(
    field_leaderboard_score,
    field_current_user
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {


    /** Returns the Abstract Syntax Tree JSON object for specified script */
    case "ast of" =>
      val List(id) = parameters // String
      TopWithInvalid[S](TJson_Object, "script may not exist or infrastructure unreachable")

    /** [**obsolete**] Opens the leaderboard for the current script */
    case "open leaderboard" =>
      Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "open leaderboard",
        "Check if the device is connected to the internet before opening the leaderboard")
      Skip

    /** [**obsolete**] Opens the review page for the current script */
    case "open review" =>
      Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "open review",
        "Check if the device is connected to the internet before opening the review page")
      Skip

    /** [**obsolete**] Launches the bazaar. */
    case "open" =>
      Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "open",
        "Check if the device is connected to the internet before launching the bazaaar")
      Skip

    /** Asks the user to pick a script and return its identifier */
    case "pick script" =>
      val List(mode, message) = parameters // String,String
      TopWithInvalid[S](TString, "use may abort script selection dialog")

    /** Posts the current game score to the script leaderboard */
    case "post leaderboard score" =>
      val List(score) = parameters // Number
      AssignField[S](this0, SBazaar.field_leaderboard_score, score)

    /** Posts the current game leaderboard to the wall */
    case "post leaderboard to wall" =>
      Skip

    /** Saves given Abstract Syntax Tree as a script */
    case "save ast" =>
      val List(id, script) = parameters // String,Json_Object
      Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "save ast",
        "Check if the device is connected to the internet")
      Skip

    /** Returns a user object for a specified user id */
    case "user of" =>
      val List(id) = parameters // String
      TopWithInvalid[S](TUser, "user id may not exist")

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
