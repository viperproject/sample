
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{MethodSummaries, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{TouchType, TypeList}
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * This implements helper functions that I use for the analysis
 *
 * @author Lucas Brutschy
 */

object SHelpers extends ASingleton {

  lazy val typeName = TypeName("Helpers",isSingleton = true)

  val CreateMethod = """create (TypeName\(.*\)) (__handler_.+)""".r

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates an action with the given name */
    case CreateMethod(handlerTyp,handlerName) =>
      MethodSummaries.collectClosureEntry(handlerName,state)
      val tN = TypeName.parseCode(handlerTyp)
      val t = TypeList.getTypeOrFail(tN)
      New[S](t,Map(AAction.field_handlerName -> String(handlerName)))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)


  }
}
      
