/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName

object TUnfinished_Type extends AAny {

  override lazy val typeName: TypeName = TypeName("Unfinished Type")

  override def getDeclaration(s:String) =
    TypeList.getType(if (CFGGenerator.isRecordIdent(s)) CFGGenerator.makeRecordTypeName(s) else TypeName(s)) match {
      case Some(x) => Some(ApiMember(s,Nil,ApiParam(this),x,DefaultSemantics))
      case None => super.getDeclaration(s)
    }

}
