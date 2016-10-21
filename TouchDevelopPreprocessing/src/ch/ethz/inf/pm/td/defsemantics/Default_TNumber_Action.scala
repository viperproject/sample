
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
 * Specifies the abstract semantics of Number Action
 *
 * A place to hook up an action to execute in response to an event
 *
 * @author Lucas Brutschy
 */

trait Default_TNumber_Action extends AAction {

  lazy val typeName = TypeName("Number Action")
          
  override def actionArguments = List(ApiParam(TNumber))
  override def actionReturnValue: AAny = TNothing


}
          
