/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TLocation
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 *
 * This is exactly the same as the GeoCoordinate class in C#
 * http://msdn.microsoft.com/en-us/library/system.device.location.geocoordinate.aspx
 *
 * Everything but lat/long is INVALID most of the time
 *
 *
 * User: Lucas Brutschy
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TLocation extends Default_TLocation {

  /** Gets the latitude of the coordinate */
  lazy val field_latitude = ApiField("latitude", TNumber) // -90 ndTo 90

  /** Gets the longitude of the coordinate */
  lazy val field_longitude = ApiField("longitude", TNumber) // -180 ndTo 180

  /** Gets the altitude of the coordinate */
  lazy val field_altitude = ApiField("altitude", TNumber)//, Invalid(TNumber))

  /** Gets the speed of the coordinate */
  lazy val field_speed = ApiField("speed", TNumber)//, Invalid(TNumber))

  /** Gets the course of the coordinate, in degrees relative to true north */
  lazy val field_course = ApiField("course", TNumber)//, Invalid(TNumber)) // 0 ndTo 360

  /** Gets the horizontal accuracy of the coordinate */
  lazy val field_hor_accuracy = ApiField("hor accuracy", TNumber)//, Invalid(TNumber))

  /** Gets the vertical accuracy of the coordinate */
  lazy val field_vert_accuracy = ApiField("vert accuracy", TNumber)//, Invalid(TNumber))

  override def possibleFields = super.possibleFields ++ List(field_latitude,field_longitude,field_altitude,
      field_speed, field_hor_accuracy,field_course,field_vert_accuracy)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Calculates the distance in meters */
    case "distance" =>
      val List(other) = parameters // Location
      Dummy[S](this0,method)
      Top[S](TNumber)

    /** Indicates if this instance is equal to the other */
    case "equals" =>
      val List(other) = parameters // Location
      Dummy[S](this0,method)
      Top[S](TBoolean)

    /** Displays the location in a map using Bing. */
    case "post to wall" =>
      Skip

    /** Shares the location (email, sms, facebook, social or '' to pick from a list) */
    case "share" =>
      val List(network,message) = parameters // String,String
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}

