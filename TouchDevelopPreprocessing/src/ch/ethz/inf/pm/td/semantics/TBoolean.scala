/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TBoolean
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TBoolean extends Default_TBoolean {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet],
                                               returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {


    /** Builds conjunction */
    case "and" =>
      val List(right) = parameters // Boolean
      Return(this0 && right)

    /** Indicates that the two values are equal */
    case "equals" =>
      val List(right) = parameters // Boolean
      Return[S]((this0 && right) || (this0.not() && right.not()))

    /** Negates the boolean expression */
    case "not" =>
      Return(this0.not())

    /** Builds disjunction */
    case "or" =>
      val List(right) = parameters // Boolean
      Return(this0 || right)

    /** Converts the value into a json data structure. */
    case "to json" =>
      If[S](this0, Then = {
        New[S](TJson_Object, initials = Map(
          TJson_Object.field_to_boolean -> True,
          TJson_Object.field_to_string -> String("true"),
          TJson_Object.field_to_time -> Invalid(TDateTime, "value may have been converted from a boolean"),
          TJson_Object.field_to_number -> 1,
          TJson_Object.field_kind -> String("boolean")
        ))(_, pp)
      }, Else = {
        New[S](TJson_Object, initials = Map(
          TJson_Object.field_to_boolean -> False,
          TJson_Object.field_to_string -> String("false"),
          TJson_Object.field_to_time -> Invalid(TDateTime, "value may have been converted from a boolean"),
          TJson_Object.field_to_number -> 0,
          TJson_Object.field_kind -> String("boolean")
        ))(_, pp)
      })

    /** Converts true to 1 and false to 0 */
    case "to number" =>
      If[S](this0, Then = {
        Return[S](1)(_, pp)
      }, Else = {
        Return[S](0)(_, pp)
      })

    /** Converts a boolean to a string */
    case "to string" =>
      If[S](this0, Then = {
        Return[S](String("true"))(_, pp)
      }, Else = {
        Return[S](String("false"))(_, pp)
      })

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

}