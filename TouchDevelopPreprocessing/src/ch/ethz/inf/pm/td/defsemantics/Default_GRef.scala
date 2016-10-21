
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
 * Specifies the abstract semantics of Ref
 *
 * A reference to a value
 *
 * @author Lucas Brutschy
 */

trait Default_GRef extends AAny {

  def TT:AAny

  lazy val typeName = TypeName("Ref", List(TT.typeName))
          
  /** Never used: Add specified value to given reference */
  override def member__add = ApiMember(
    name = "◈add",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set reference to invalid */
  override def member__clear = ApiMember(
    name = "◈clear",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Checks if value is confirmed */
  override def member__confirmed = ApiMember(
    name = "◈confirmed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Get the current value of the reference */
  override def member__get = ApiMember(
    name = "◈get",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TT,
    semantics = DefaultSemantics
  )

  /** Never used: Set the value of the reference */
  override def member__set = ApiMember(
    name = "◈set",
    paramTypes = List(ApiParam(TT)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set reference to `v` if it's currently non-empty */
  override def member__test_and_set = ApiMember(
    name = "◈test and set",
    paramTypes = List(ApiParam(TT)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Create a new ref, that invokes `on changed` whenever the update is performed through it */
  def member__with_notify = ApiMember(
    name = "◈with notify",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = GRef(TT),
    semantics = DefaultSemantics
  )

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "◈add" -> member__add,
    "◈clear" -> member__clear,
    "◈confirmed" -> member__confirmed,
    "◈get" -> member__get,
    "◈set" -> member__set,
    "◈test and set" -> member__test_and_set,
    "◈with notify" -> member__with_notify
  )
            

}
          
