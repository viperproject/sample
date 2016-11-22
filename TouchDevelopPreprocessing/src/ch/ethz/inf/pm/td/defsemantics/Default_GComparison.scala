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
 * Specifies the abstract semantics of Comparison
 *
 * An atomic comparison action
 *
 * @author Lucas Brutschy
 */

trait Default_GComparison extends AAction {

  def TElt:AAny

  lazy val typeName = TypeName("Comparison", List(TElt.typeName))

  override def actionReturnValue: AAny = TNumber
  override def actionArguments: List[ApiParam] = List(ApiParam(TElt), ApiParam(TElt))

}
          
