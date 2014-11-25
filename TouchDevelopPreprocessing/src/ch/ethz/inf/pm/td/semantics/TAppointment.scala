package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TopWithInvalidInitializer, ApiField}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Appointment
 *
 * An calendar appointment
 *
 * @author Lucas Brutschy
 */

object TAppointment extends AAny {

  /** Gets the list of attendees. Each contact contains a name and email address. */
  lazy val field_attendees = new ApiField("attendees", TContact_Collection.typeName)

  /** Gets the details */
  lazy val field_details = new ApiField("details", TString.typeName, topDefault = TopWithInvalidInitializer("appointment may not have details"))

  /** Gets the end time */
  lazy val field_end_time = new ApiField("end time", TDateTime.typeName)

  /** Indicates if this is an all day event */
  lazy val field_is_all_day_event = new ApiField("is all day event", TBoolean.typeName)

  /** Indicates if this appointment is private */
  lazy val field_is_private = new ApiField("is private", TBoolean.typeName)

  /** Gets the location */
  lazy val field_location = new ApiField("location", TString.typeName, topDefault = TopWithInvalidInitializer("appointment may not have a location"))

  /** Gets the organizer */
  lazy val field_organizer = new ApiField("organizer", TContact.typeName)

  /** Gets the source of this appointment (facebook, etc...) */
  lazy val field_source = new ApiField("source", TString.typeName)

  /** Gets the location */
  lazy val field_start_time = new ApiField("start time", TDateTime.typeName)

  /** Gets your status (free, tentative, busy, outofoffice) */
  lazy val field_status = new ApiField("status", TString.typeName)

  /** Gets the subject */
  lazy val field_subject = new ApiField("subject", TString.typeName, topDefault = TopWithInvalidInitializer("appointment may not have a subject"))

  lazy val typeName = TypeName("Appointment")

  override def possibleFields = super.possibleFields ++ List(field_attendees, field_details, field_end_time,
    field_is_all_day_event, field_is_private, field_location, field_organizer, field_source, field_start_time, field_status, field_subject)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {
    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}