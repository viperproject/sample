/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.oorepresentation.Modifier
import ch.ethz.inf.pm.td.compiler.TypeList
import ch.ethz.inf.pm.td.parser.TypeName

case class GDecorator(typeName:TypeName, keyTypeName:TypeName, valueType:AAny, modifiers:Set[Modifier]) extends AIndex {

  lazy val keyType:AAny = TypeList.toTouchType(keyTypeName)

}
