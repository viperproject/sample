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
 * Specifies the abstract semantics of Appointment
 *
 * An calendar appointment
 *
 * @author Lucas Brutschy
 */

trait Default_TAppointment extends AAny {

  lazy val typeName = TypeName("Appointment")
          
  /** Rarely used: Gets the list of attendees. Each contact contains a name and email address. */
  def member_attendees = ApiMember(
    name = "attendees",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TContact),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the details */
  def member_details = ApiMember(
    name = "details",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the end time */
  def member_end_time = ApiMember(
    name = "end time",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates if this is an all day event */
  def member_is_all_day_event = ApiMember(
    name = "is all day event",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if this appointment is private */
  def member_is_private = ApiMember(
    name = "is private",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the location */
  def member_location = ApiMember(
    name = "location",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the organizer */
  def member_organizer = ApiMember(
    name = "organizer",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TContact,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the source of this appointment (facebook, etc...) */
  def member_source = ApiMember(
    name = "source",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the location */
  def member_start_time = ApiMember(
    name = "start time",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets your status (free, tentative, busy, outofoffice) */
  def member_status = ApiMember(
    name = "status",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the subject */
  def member_subject = ApiMember(
    name = "subject",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "attendees" -> member_attendees,
    "details" -> member_details,
    "end time" -> member_end_time,
    "is all day event" -> member_is_all_day_event,
    "is private" -> member_is_private,
    "location" -> member_location,
    "organizer" -> member_organizer,
    "source" -> member_source,
    "start time" -> member_start_time,
    "status" -> member_status,
    "subject" -> member_subject
  )
            

}
          
