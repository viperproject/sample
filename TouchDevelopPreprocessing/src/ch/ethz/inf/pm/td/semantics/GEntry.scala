/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Key / Value pair for collections
 */
case class GEntry(key:AAny,value:AAny) extends AAny {

  override def typeName: TypeName = TypeName("Entry",List(key.typeName,value.typeName))

  lazy val field_key =   ApiField("*key", key)
  lazy val field_value = ApiField("*value", value)

  override def possibleFields = super.possibleFields ++ List(field_key,field_value)

}
