/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TopWithInvalidInitializer, ApiField}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TAppointment
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Appointment
 *
 * An calendar appointment
 *
 * @author Lucas Brutschy
 */

object TAppointment extends Default_TAppointment {

  /** Gets the list of attendees. Each contact contains a name and email address. */
  lazy val field_attendees = ApiField("attendees", GCollection(TContact))

  /** Gets the details */
  lazy val field_details = new ApiField("details", TString, topDefault = TopWithInvalidInitializer("appointment may not have details"))

  /** Gets the end time */
  lazy val field_end_time = ApiField("end time", TDateTime)

  /** Indicates if this is an all day event */
  lazy val field_is_all_day_event = ApiField("is all day event", TBoolean)

  /** Indicates if this appointment is private */
  lazy val field_is_private = ApiField("is private", TBoolean)

  /** Gets the location */
  lazy val field_location = new ApiField("location", TString, topDefault = TopWithInvalidInitializer("appointment may not have a location"))

  /** Gets the organizer */
  lazy val field_organizer = ApiField("organizer", TContact)

  /** Gets the source of this appointment (facebook, etc...) */
  lazy val field_source = ApiField("source", TString)

  /** Gets the location */
  lazy val field_start_time = ApiField("start time", TDateTime)

  /** Gets your status (free, tentative, busy, outofoffice) */
  lazy val field_status = ApiField("status", TString)

  /** Gets the subject */
  lazy val field_subject = new ApiField("subject", TString, topDefault = TopWithInvalidInitializer("appointment may not have a subject"))

  override def possibleFields = super.possibleFields ++ List(field_attendees, field_details, field_end_time,
    field_is_all_day_event, field_is_private, field_location, field_organizer, field_source, field_start_time, field_status, field_subject)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {
    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}