
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
 * Specifies the abstract semantics of Page Button
 *
 * A page button on the wall
 *
 * @author Lucas Brutschy
 */

trait Default_TPage_Button extends AAny {

  lazy val typeName = TypeName("Page Button")
          
  /** Frequently used: Gets the icon name */
  def member_icon = ApiMember(
    name = "icon",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the page hosting this button */
  def member_page = ApiMember(
    name = "page",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPage,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the text */
  def member_text = ApiMember(
    name = "text",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "icon" -> member_icon,
    "page" -> member_page,
    "text" -> member_text
  )
            

}
          
