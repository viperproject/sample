
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
 * Specifies the abstract semantics of Contract
 *
 * Correctness helpers
 *
 * @author Lucas Brutschy
 */

trait Default_SContract extends ASingleton {

  lazy val typeName = TypeName("Contract", isSingleton = true)
          
  /** Never used: Checks for a condition; if the condition is false, execution fails. Does nothing for published scripts. */
  def member_assert = ApiMember(
    name = "assert",
    paramTypes = List(ApiParam(TBoolean), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Specifies a precondition contract for the action; if the condition is false, execution fails. Does nothing for published scripts. */
  def member_requires = ApiMember(
    name = "requires",
    paramTypes = List(ApiParam(TBoolean), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "assert" -> member_assert,
    "requires" -> member_requires
  )
            

}
          
