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
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SLocations
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/29/12
 * Time: 12:44 PM
 */

object SLocations extends Default_SLocations {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a new geo coordinate location */
    case "create location" =>
      val List(latitude,longitude) = parameters // Number,Number
      val res = New[S](TLocation,Map(
        TLocation.field_latitude -> latitude,
        TLocation.field_longitude -> longitude
      ))
      res

    /** Creates an empty list of locations */
    case "create location list" =>
      New[S](GCollection(TLocation))

    /** Looks for an address near a location using Bing. */
    case "describe location" =>
      val List(location) = parameters // Location
      val ret = Top[S](TString)
      ret

    /** Looks for the coordinate of an address using Bing. */
    case "search location" =>
      val List(address,postal_code,city,country) = parameters // String,String,String,String
      New[S](TLocation)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}