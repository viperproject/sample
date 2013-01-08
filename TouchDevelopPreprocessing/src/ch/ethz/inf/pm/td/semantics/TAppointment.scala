package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Appointment
 *
 * An calendar appointment
 *
 * @author Lucas Brutschy
 */

object TAppointment {

   /** Gets the list of attendees. Each contact contains a name and email address. */
   val field_attendees = new TouchField("attendees",TContact_Collection.typ)

   /** Gets the details */
   val field_details = new TouchField("details",TString.typ)

   /** Gets the end time */
   val field_end_time = new TouchField("end_time",TDateTime.typ)

   /** Indicates if this is an all day event */
   val field_is_all_day_event = new TouchField("is_all_day_event",TBoolean.typ)

   /** Indicates if this appointment is private */
   val field_is_private = new TouchField("is_private",TBoolean.typ)

   /** Gets the location */
   val field_location = new TouchField("location",TString.typ)

   /** Gets the organizer */
   val field_organizer = new TouchField("organizer",TContact.typ)

   /** Gets the source of this appointment (facebook, etc...) */
   val field_source = new TouchField("source",TString.typ)

   /** Gets the location */
   val field_start_time = new TouchField("start_time",TDateTime.typ)

   /** Gets your status (free, tentative, busy, outofoffice) */
   val field_status = new TouchField("status",TString.typ)

  /** Gets the subject */
  val field_subject = new TouchField("subject",TString.typ)

  val typName = "Appointment"
  val typ = TouchType(typName,isSingleton = false,List(field_attendees,field_details,field_end_time,
    field_is_all_day_event,field_is_private,field_location,field_organizer,field_source,field_start_time,field_status,field_subject))

}

class TAppointment extends AAny {

  def getTyp = TAppointment.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {
    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}