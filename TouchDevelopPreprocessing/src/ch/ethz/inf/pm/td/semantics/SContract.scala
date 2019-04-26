/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SContract
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Contract
 *
 * Correctness helpers
 *
 * @author Lucas Brutschy
 */ 

object SContract extends Default_SContract {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Specifies a precondition contract for the action; if the condition is false, execution fails. Does nothing for published scripts. */
    case "requires" =>
      val List(condition,message) = parameters // Boolean,String
      if (TouchAnalysisParameters.get.printValuesInWarnings)
        Error[S](condition.not(), "requires", "Precondition "+condition+" may  not be satisfied!")
      else
        Error[S](condition.not(), "requires", "Precondition may not be satisfied!")

    /** Checks for a condition; if the condition is false, execution fails. Does nothing for published scripts. */
    case "assert" =>
      val List(condition,message) = parameters // Boolean,String
      if (TouchAnalysisParameters.get.printValuesInWarnings)
        Error[S](condition.not(), "assert", "Assertion "+condition+" may not hold!")
      else
        Error[S](condition.not(), "assert", "Assertion may not hold!")

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
