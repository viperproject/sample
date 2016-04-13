
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Task
 *
 * A task created with `async` keyword
 *
 * @author Lucas Brutschy
 */

trait Default_GTask extends AAny {

  def TT:AAny
           

  lazy val typeName = TypeName("Task", List(TT.typeName))
          
  /** Never used: Wait for the task to finish for at most `seconds`; returns invalid in case of timeout */
  def member_await_at_most = ApiMember(
    name = "await at most",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TT,
    semantics = DefaultSemantics
  )

  /** Never used: Wait for the task to finish, and return any possible value */
  def member_await = ApiMember(
    name = "await",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TT,
    semantics = DefaultSemantics
  )

  /** Never used: Check if the task is done yet */
  def member_completed = ApiMember(
    name = "completed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Get the value of the task, which must have completed. */
  def member_value = ApiMember(
    name = "value",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TT,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "await at most" -> member_await_at_most,
    "await" -> member_await,
    "completed" -> member_completed,
    "value" -> member_value
  )
            

}
          
