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
    TypeList.getType(if (CFGGenerator.isRecordIdent(s)) CFGGenerator.makeRecordTypeName(s) else TypeName(s)) match {
      case Some(x) => Some(ApiMember(s,Nil,ApiParam(this),GCollection(x),NewSemantics))
      case None =>
        s match {
          case "Ref of" => Some(ApiMember(s,Nil,ApiParam(this),TUnfinished_Ref_Collection,NewSemantics))
          case _ =>
            super.getDeclaration(s)
        }
    }

}
