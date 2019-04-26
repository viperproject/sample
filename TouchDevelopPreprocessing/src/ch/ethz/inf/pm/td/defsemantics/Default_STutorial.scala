/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiParam, DefaultSemantics}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Tutorial
 *
 * Support for interactive tutorials.
 *
 * @author Lucas Brutschy
 */

trait Default_STutorial extends ASingleton {

  lazy val typeName = TypeName("Tutorial", isSingleton = true)

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "show hint" -> member_show_hint,
    "step completed" -> member_step_completed
  )

  /** Never used: [**dbg**] Show a suggestion to the user (eg., an error description) */
  def member_show_hint = ApiMember(
    name = "show hint",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Signal that the step is done. */
  def member_step_completed = ApiMember(
    name = "step completed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )
            

}
          
