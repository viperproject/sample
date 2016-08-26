/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.defsemantics.Default_TNumber
import RichNativeSemantics._
import ch.ethz.inf.pm.td.cloud.CloudUpdateWrapper

/**
 * Specifies the abstract semantics of Number
 *
 * A number (possibly negative and/or fractional)
 *
 * @author Lucas Brutschy
 */
object TNumber extends Default_TNumber {

  override lazy val member_to_string = super.member_to_string.copy(semantics = ValidPureSemantics)

  def member__add = ApiMember(
    name = "◈add",
    paramTypes = List(ApiParam(TNumber,isMutated = false)),
    thisType = ApiParam(this, isMutated = true),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Assign[S](this0,this0 + parameters.head)
      }
    },Set(CloudEnabledModifier))
  )
  override def declarations: Map[String, ApiMember] = super.declarations + (member__add.name -> member__add)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String,parameters:List[ExpressionSet],returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    case "≥" => Return(this0 >= parameters.head)
    case "≤" => Return(this0 <= parameters.head)
    case "=" => Return(this0 equal parameters.head)
    case "≠" => Return(this0 unequal parameters.head)
    case ">" => Return(this0 > parameters.head)
    case "<" => Return(this0 < parameters.head)
    case "+" => Return(this0 + parameters.head)
    case "*" => Return(this0 * parameters.head)
    case "-" => Return(this0 - parameters.head)
    case "/" =>
      if (TouchAnalysisParameters.get.reportNumericalErrors)
        Error[S](parameters.head equal 0, "Possible division by zero")
      Return(this0 / parameters.head)

    case "and" => Return((this0 unequal 0) && parameters.head)
    case "or" => Return((this0 unequal 0) || parameters.head)
    case "not" => Return(this0 equal 0)

    /** Interprets a number as a unicode value and converts it to the single character string */
    case "to character" =>
      Top[S](TString)

    /** Interprets the number as a ARGB (alpha, red, green, blue) color */
    case "to color" =>
      Top[S](TColor) // TODO: This should be possible to implement

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
