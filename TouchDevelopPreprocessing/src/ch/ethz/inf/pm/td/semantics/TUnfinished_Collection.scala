/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName

object TUnfinished_Collection extends AAny {

  override lazy val typeName: TypeName = TypeName("Unfinished Collection")

  override def getDeclaration(s:String) =
    TypeList.getType(TypeName(if (CFGGenerator.isRecordIdent(s)) CFGGenerator.getRecordName(s) else s)) match {
      case Some(x) => Some(ApiMember(s,Nil,ApiParam(this),GCollection(x),NewSemantics))
      case None => super.getDeclaration(s)
    }

}
