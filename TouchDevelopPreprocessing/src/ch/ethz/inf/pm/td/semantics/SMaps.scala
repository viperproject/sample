/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SMaps
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/26/12
 * Time: 1:09 PM
 */

object SMaps extends Default_SMaps {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Creates a full screen Bing map. Use 'post to wall' to display it. */
    case "create full map" =>
      New[S](TMap) // PRECISION: We might want to set zoom/center here

    /** Creates a Bing map. Use 'post to wall' to display it. */
    case "create map" =>
      New[S](TMap) // PRECISION: We might want to set zoom/center here

    /** Calculates the directions between two coordinates using Bing. */
    case "directions" =>
      val List(from, to, walking) = parameters // Location,Location,Boolean
      if (TouchAnalysisParameters.get.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "directions",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](GCollection(TLocation), "direction service may be unreachable")

    /** Shows the directions in the Bing map application. If search term is provided, location is ignored.
        Provide search term or location for start and end. */
    case "open directions" =>
      val List(start_search, start_loc, end_search, end_loc) = parameters // String,Location,String,Location
      Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "open directions",
        "Check if the device is connected to the internet before opening the directions")
      Skip

    /** Opens the Bing map application. zoom between 0 (close) and 1 (far). */
    case "open map" =>
      val List(center, search, zoom) = parameters // Location,String,Number
      Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "open map",
        "Check if the device is connected to the internet before opening a map")
      if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](zoom, 0, 1, method, "zoom")
      }
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
