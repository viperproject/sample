/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{CFGGenerator, TouchType}
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * This is empty but needs to be there as a type
 *
 * @author Lucas Brutschy
 */

object SArt extends ASingleton {

  lazy val typeName = TypeName("art",isSingleton = true)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = {

    SData.forwardSemantics[S](SData.reference,method,parameters,returnedType)

  }

}
