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
 * Specifies the abstract semantics of Sprite Set
 *
 * A collection of sprites
 *
 * @author Lucas Brutschy
 */

trait Default_TSprite_Set extends AMutableLinearCollection {

  lazy val typeName = TypeName("Sprite Set")
          
  def keyType = TNumber

  def valueType = TSprite

  /** Sometimes used: Add sprite to set and remove from old set. Returns true if sprite was in old set and not in new set. */
  def member_add_from = ApiMember(
    name = "add from",
    paramTypes = List(ApiParam(TSprite_Set,isMutated=true), ApiParam(TSprite)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Remove sprite that was added to set first. */
  def member_remove_first = ApiMember(
    name = "remove first",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TSprite,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add from" -> member_add_from,
    "remove first" -> member_remove_first
  )
            

}
          
