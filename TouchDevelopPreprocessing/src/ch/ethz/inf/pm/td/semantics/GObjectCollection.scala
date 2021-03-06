/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{Modifier, ProgramPoint}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

case class GObjectCollection(objectTyp:AAny,modifiers:Set[Modifier]) extends AMutableLinearCloudCollection {

  def typeName = TypeName("Collection", List(objectTyp.typeName))
  def keyType = TNumber
  def valueType = objectTyp

}
