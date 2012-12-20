
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of social
 *
 * Emails, sms, contacts, calendar, ...
 *
 * @author Lucas Brutschy
 */ 

object SSocial {

  val typName = "social"
  val typ = TouchType(typName,isSingleton = true,List())

}

class SSocial extends AAny {

  def getTyp = SSocial.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Chooses a contact from the contact list */
    // case "choose_contact" => 
    //   Return[S](Valid(TContact.typ))
    // DECLARATION AS FIELD: 
    //   /** Chooses a contact from the contact list */
    //   val field_choose_contact = new TouchField("choose_contact",TContact.typ)

    /** Chooses an email from the contact list */
    // case "choose_email" => 
    //   Return[S](Valid(TLink.typ))
    // DECLARATION AS FIELD: 
    //   /** Chooses an email from the contact list */
    //   val field_choose_email = new TouchField("choose_email",TLink.typ)

    /** Creates a new contact */
    // case "create_contact" => 
    //   val List(nickname) = parameters // String
    //   Return[S](Valid(TContact.typ))

    /** Creates a message to share */
    // case "create_message" => 
    //   val List(message) = parameters // String
    //   Return[S](Valid(TMessage.typ))

    /** Creates a place */
    // case "create_place" => 
    //   val List(name,location) = parameters // String,Location
    //   Return[S](Valid(TPlace.typ))

    /** Creates a link from an email */
    // case "link_email" => 
    //   val List(email_address) = parameters // String
    //   Return[S](Valid(TLink.typ))

    /** Creates a link from a phone number */
    // case "link_phone_number" => 
    //   val List(phone_number) = parameters // String
    //   Return[S](Valid(TLink.typ))

    /** Saves a new contact */
    // case "save_contact" => 
    //   val List(contact) = parameters // Contact
    //   Skip;

    /** Allows the user to save the email address (email) */
    // case "save_email" => 
    //   val List(email_address) = parameters // String
    //   Skip;

    /** Searches for recent messages in a social network (twitter, facebook) */
    // case "search" => 
    //   val List(network,terms) = parameters // String,String
    //   Return[S](Valid(TMessage_Collection.typ))

    /** Searches for appointments in a given time range */
    // case "search_appointments" => 
    //   val List(start,end) = parameters // DateTime,DateTime
    //   Return[S](Valid(TAppointment_Collection.typ))

    /** Searches for contacts by name. */
    // case "search_contacts" => 
    //   val List(prefix) = parameters // String
    //   Return[S](Valid(TContact_Collection.typ))

    /** Searches for places nearby. The distance is in meters. */
    // case "search_places_nearby" => 
    //   val List(network,terms,location,distance) = parameters // String,String,Location,Number
    //   Return[S](Valid(TPlace_Collection.typ))

    /** Opens the mail client */
    // case "send_email" => 
    //   val List(to,subject,body) = parameters // String,String,String
    //   Skip;

    /** Opens the short message client (to, body) */
    // case "send_sms" => 
    //   val List(to,body) = parameters // String,String
    //   Skip;

    // FIELDS: , field_choose_contact, field_choose_email

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
