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
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TMap
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/22/12
 * Time: 6:34 PM
 */

object TMap extends Default_TMap {

  /** Gets the zoom level */
  lazy val field_zoom = ApiField("zoom", TNumber)

  /** Gets the map center location */
  lazy val field_center = ApiField("center", TLocation)

  override def possibleFields = super.possibleFields ++ List(field_zoom, field_center)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {

    /** Adds a polyline that passes through various geocoordinates */
    case "add line" =>
      val List(locations, color, thickness) = parameters // Location_Collection,Color,Number
      Top[S](TMap_Pushpin)

    /** Adds a link pushpin on the map (ignored if the location if not set) */
    case "add link" =>
      val List(link, background, foreground) = parameters // Link,Color,Color
      Top[S](TMap_Pushpin)

    /** Adds a message pushpin on the map (ignored if the location is not set) */
    case "add message" =>
      val List(msg, background, foreground) = parameters // Message,Color,Color
      Top[S](TMap_Pushpin)

    /** Adds a picture pushpin on the map */
    case "add picture" =>
      val List(location, picture, background) = parameters // Location,Picture,Color
      Top[S](TMap_Pushpin)

    /** Adds a place pushpin on the map (ignored if the location is not set) */
    case "add place" =>
      val List(place, background, foreground) = parameters // Place,Color,Color
      Top[S](TMap_Pushpin)

    /** Adds a text pushpin on the map */
    case "add text" =>
      val List(location, text, background, foreground) = parameters // Location,String,Color,Color
      Top[S](TMap_Pushpin)

    /** Clears the lines, regions and pushpins */
    case "clear" =>
      Skip; // TODO

    /** Fills a region with a color */
    case "fill region" =>
      val List(locations, fill, stroke, thickness) = parameters // Location_Collection,Color,Color,Number
      Skip; // TODO

    /** Sets the map center location */
    case "set center" =>
      val List(center) = parameters // Location
      Skip; // TODO

    /** Sets the zoom level from 1 (earth) to 21 (street) */
    case "set zoom" =>
      val List(level) = parameters // Number
      if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](level, 1, 21, method, "level")
      }
      AssignField[S](this0, TMap.field_zoom, parameters.head)

    /** Changes the current zoom and center so that all the pushpins are visible. This method has no effect if the map is not posted on a the wall yet. */
    case "view pushpins" =>
      // TODO: Check if already posted
      // PRECISION: We could implement this
      val state1 = AssignField(this0, TMap.field_zoom, 1 ndToIncl 21)
      val state2 = Top[S](TLocation)(state1, pp)
      AssignField(this0, TMap.field_center, state2.expr)(state2, pp)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}

