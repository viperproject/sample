
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
 * Specifies the abstract semantics of Unknown
 *
 * an unknown value
 *
 * @author Lucas Brutschy
 */

trait Default_TUnknown extends AAny {

  lazy val typeName = TypeName("Unknown")
          
  /** Rarely used:  */
  def member_break_ = ApiMember(
    name = "break",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = DefaultSemantics
  )

  /** Rarely used:  */
  def member_continue_ = ApiMember(
    name = "continue",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = DefaultSemantics
  )

  /** Rarely used:  */
  def member_fun = ApiMember(
    name = "fun",
    paramTypes = List(ApiParam(TUnknown)),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = DefaultSemantics
  )

  /** Rarely used:  */
  def member_return_ = ApiMember(
    name = "return",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = DefaultSemantics
  )

  /** Rarely used:  */
  def member_show = ApiMember(
    name = "show",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "break" -> member_break_,
    "continue" -> member_continue_,
    "fun" -> member_fun,
    "return" -> member_return_,
    "show" -> member_show
  )
            

}
          
