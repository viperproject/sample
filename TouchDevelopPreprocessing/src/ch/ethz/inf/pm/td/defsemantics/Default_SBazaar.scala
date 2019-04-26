/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Bazaar
 *
 * Browse and review scripts from the bazaar
 *
 * @author Lucas Brutschy
 */

trait Default_SBazaar extends ASingleton {

  lazy val typeName = TypeName("Bazaar", isSingleton = true)

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
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

  /** Never used: Returns the Abstract Syntax Tree JSON object for specified script */
  def member_ast_of = ApiMember(
    name = "ast of",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the user object of the current user */
  def member_current_user = ApiMember(
    name = "current user",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TUser,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the current score for the current script */
  def member_leaderboard_score = ApiMember(
    name = "leaderboard score",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    isAsync = true,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] three-way merge script texts. Debug only: for testing. */
  def member_merge3 = ApiMember(
    name = "merge3",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Opens the leaderboard for the current script */
  def member_open_leaderboard = ApiMember(
    name = "open leaderboard",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Opens the review page for the current script */
  def member_open_review = ApiMember(
    name = "open review",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Launches the bazaar. */
  def member_open = ApiMember(
    name = "open",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Asks the user to pick a script and return its identifier */
  def member_pick_script = ApiMember(
    name = "pick script",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Posts the current game score to the script leaderboard */
  def member_post_leaderboard_score = ApiMember(
    name = "post leaderboard score",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    isAsync = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Posts the current game leaderboard to the wall */
  def member_post_leaderboard_to_wall = ApiMember(
    name = "post leaderboard to wall",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Saves given Abstract Syntax Tree as a script */
  def member_save_ast = ApiMember(
    name = "save ast",
    paramTypes = List(ApiParam(TString), ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Returns an identifier of either the top-level script or the current library */
  def member_script_id = ApiMember(
    name = "script id",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Returns a user object for a specified user id */
  def member_user_of = ApiMember(
    name = "user of",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TUser,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )
            

}
          
