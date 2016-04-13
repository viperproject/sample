/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.oorepresentation.Modifier
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.parser.TypeName

case class GSingletonIndex(indexMemberType:AAny,modifiers:Set[Modifier]) extends AAny {

  lazy val field_singleton = new ApiField("singleton", indexMemberType)

  def typeName = TypeName("Index",List(indexMemberType.typeName))

  override def possibleFields = super.possibleFields + field_singleton

}
