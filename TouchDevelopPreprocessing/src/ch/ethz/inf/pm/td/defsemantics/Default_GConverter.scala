/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Converter
 *
 * A generic atomic conversion function
 *
 * @author Lucas Brutschy
 */

trait Default_GConverter extends AAction {

  lazy val typeName = TypeName("Converter", List(TFrom.typeName, TTo.typeName))

  def TFrom:AAny

  def TTo:AAny

  override def actionArguments: List[ApiParam] = List(ApiParam(TFrom))

  override def actionReturnValue: AAny = TTo


}
          
