/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.defsemantics.Default_GConverter

/**
 * Customizes the abstract semantics of Converter
 *
 * A generic atomic conversion function
 *
 * @author Lucas Brutschy
 */

case class GConverter (TFrom:AAny, TTo:AAny) extends Default_GConverter {
          

}
          
