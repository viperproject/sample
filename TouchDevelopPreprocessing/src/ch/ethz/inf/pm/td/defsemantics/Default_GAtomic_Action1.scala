
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
 * Specifies the abstract semantics of Atomic Action1
 *
 * An atomic single argument action
 *
 * @author Lucas Brutschy
 */

trait Default_GAtomic_Action1 extends AAction {

  def TT:AAny

  lazy val typeName = TypeName("Atomic Action1", List(TT.typeName))
          
  override def actionArguments = List(ApiParam(TT))
  override def actionReturnValue: AAny = TNothing

}
          
