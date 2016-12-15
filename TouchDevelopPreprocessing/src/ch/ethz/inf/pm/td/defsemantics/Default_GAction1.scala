/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.ApiParam
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Action1
 *
 * A possibly non-atomic single argument action
 *
 * @author Lucas Brutschy
 */

trait Default_GAction1 extends AAction {

  lazy val typeName = TypeName("Action1", List(TT.typeName))

  def TT: AAny

  override def actionArguments: List[ApiParam] = List(ApiParam(TT))
  override def actionReturnValue: AAny = TNothing


}
          
