
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
 * Specifies the abstract semantics of Enumerator
 *
 * An general enumerator
 *
 * @author Lucas Brutschy
 */

trait Default_TEnumerator extends AAny {

  lazy val typeName = TypeName("Enumerator")
          
  /** Never used: [**not implemented**] Return current value */
  def member_current = ApiMember(
    name = "current",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] Advance enumerator and return true if there is another element. */
  def member_move_next = ApiMember(
    name = "move next",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "current" -> member_current,
    "move next" -> member_move_next
  )
            

}
          
