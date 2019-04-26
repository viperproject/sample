/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Motion
 *
 * Describes the motion of the device
 *
 * @author Lucas Brutschy
 */

trait Default_TMotion extends AAny {

  lazy val typeName = TypeName("Motion")
          
  /** Never used: Gets the linear acceleration of the device, in gravitational units. */
  def member_acceleration = ApiMember(
    name = "acceleration",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the gravity vector associated with this reading. */
  def member_gravity = ApiMember(
    name = "gravity",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the pitch of the attitude in degrees */
  def member_pitch = ApiMember(
    name = "pitch",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the roll of the attitude in degrees */
  def member_roll = ApiMember(
    name = "roll",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the device rotation speed in degrees per sec. */
  def member_rotation_speed = ApiMember(
    name = "rotation speed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a timestamp indicating the time at which the reading was calculated. */
  def member_time = ApiMember(
    name = "time",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the yaw of the attitude in degrees */
  def member_yaw = ApiMember(
    name = "yaw",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "acceleration" -> member_acceleration,
    "gravity" -> member_gravity,
    "pitch" -> member_pitch,
    "roll" -> member_roll,
    "rotation speed" -> member_rotation_speed,
    "time" -> member_time,
    "yaw" -> member_yaw
  )
            

}
          
