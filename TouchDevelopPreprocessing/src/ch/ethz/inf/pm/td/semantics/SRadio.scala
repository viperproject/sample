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
import ch.ethz.inf.pm.td.analysis.{TopInitializer, NewInitializer, ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SRadio
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of radio
 *
 * Access to the radio
 *
 * @author Lucas Brutschy
 */ 

object SRadio extends Default_SRadio {

  /** Gets the frequency */
  lazy val field_frequency = ApiField("frequency", TNumber, TopInitializer)

  /** Indicates if the radio is on */
  lazy val field_is_playing = ApiField("is playing", TBoolean, TopInitializer)

  override def possibleFields = super.possibleFields ++ List(field_frequency, field_is_playing)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a link to a radio frequency */
    case "link frequency" =>
      val List(name,frequency) = parameters // String,Number
      New[S](TLink,Map(
        TLink.field_name-> toRichExpression(name),
        TLink.field_kind -> String("radio")
      ))

    /** Gets the signal strength */
    case "signal strength" =>
      Top[S](TNumber)

    /** Turns on the radio */
    case "start" =>
      AssignField[S](this0,SRadio.field_is_playing,True)

    /** Turns off the radio */
    case "stop" =>
      AssignField[S](this0,SRadio.field_is_playing,True)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
