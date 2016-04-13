/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.oorepresentation.Modifier
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName

case class GObjectConstructor(objectTyp:AAny,modifiers:Set[Modifier]) extends AAny {

  def typeName = TypeName("Constructor", List(objectTyp.typeName))

  def member_create = ApiMember(
    name = "create",
    paramTypes = Nil,
    thisType = ApiParam(this),
    returnType = objectTyp,
    semantics = NewSemantics
  )

  def member_create_collection = ApiMember(
    name = "create collection",
    paramTypes = Nil,
    thisType = ApiParam(this),
    returnType = GObjectCollection(objectTyp,modifiers),
    semantics = NewSemantics
  )

  def member_invalid = ApiMember(
    name = "invalid",
    paramTypes = Nil,
    thisType = ApiParam(this),
    returnType = objectTyp,
    semantics = InvalidSemantics
  )

  override lazy val declarations = super.declarations ++
    Set(member_invalid,member_create,member_create_collection).map{x => x.name -> x}

}
