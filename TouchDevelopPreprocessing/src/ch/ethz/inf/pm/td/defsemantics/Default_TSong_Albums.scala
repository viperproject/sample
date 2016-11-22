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
 * Specifies the abstract semantics of Song Albums
 *
 * A collection of albums
 *
 * @author Lucas Brutschy
 */

trait Default_TSong_Albums extends ALinearCollection {

  lazy val typeName = TypeName("Song Albums")
          
  def keyType = TNumber

  def valueType = TSong_Album


}
          
