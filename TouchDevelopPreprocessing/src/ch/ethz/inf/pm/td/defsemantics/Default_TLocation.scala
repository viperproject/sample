/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Location
 *
 * A geo coordinate (latitude, longitude, ...)
 *
 * @author Lucas Brutschy
 */

trait Default_TLocation extends AAny {

  lazy val typeName = TypeName("Location")
          
  /** Rarely used: Gets the altitude of the coordinate */
  def member_altitude = ApiMember(
    name = "altitude",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the course of the coordinate */
  def member_course = ApiMember(
    name = "course",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Calculates the distance in meters */
  def member_distance = ApiMember(
    name = "distance",
    paramTypes = List(ApiParam(TLocation)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the horizontal accuracy of the coordinate */
  def member_hor_accuracy = ApiMember(
    name = "hor accuracy",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the latitude of the coordinate */
  def member_latitude = ApiMember(
    name = "latitude",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the longitude of the coordinate */
  def member_longitude = ApiMember(
    name = "longitude",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Shares the location (email, sms, social or &#39;&#39; to pick from a list) */
  def member_share = ApiMember(
    name = "share",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the speed of the coordinate */
  def member_speed = ApiMember(
    name = "speed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Converts to a string lat,long */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the vertical accuracy of the coordinate */
  def member_vert_accuracy = ApiMember(
    name = "vert accuracy",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "altitude" -> member_altitude,
    "course" -> member_course,
    "distance" -> member_distance,
    "hor accuracy" -> member_hor_accuracy,
    "latitude" -> member_latitude,
    "longitude" -> member_longitude,
    "share" -> member_share,
    "speed" -> member_speed,
    "to string" -> member_to_string,
    "vert accuracy" -> member_vert_accuracy
  )
            

}
          
