
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.semantics.RichExpression._

/**
 * Specifies the abstract semantics of social
 *
 * Emails, sms, contacts, calendar, ...
 *
 * @author Lucas Brutschy
 */ 

object SSocial {

  val typName = "social"
  val typ = new TouchType(typName,isSingleton = true,List())

}

class SSocial extends AAny {

  def getTyp = SSocial.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Chooses a contact from the contact list */
    // case "choose_contact" => 
    //   Top[S](TContact.typ)
    // DECLARATION AS FIELD: 
    //   /** Chooses a contact from the contact list */
    //   val field_choose_contact = new TouchField("choose_contact",TContact.typ)

    /** Chooses an email from the contact list */
    // case "choose_email" => 
    //   Top[S](TLink.typ)
    // DECLARATION AS FIELD: 
    //   /** Chooses an email from the contact list */
    //   val field_choose_email = new TouchField("choose_email",TLink.typ)

    /** Creates a new contact */
    // case "create_contact" => 
    //   val List(nickname) = parameters // String
    //   Top[S](TContact.typ)

    /** Creates a message to share */
    // case "create_message" => 
    //   val List(message) = parameters // String
    //   Top[S](TMessage.typ)

    /** Creates a place */
    // case "create_place" => 
    //   val List(name,location) = parameters // String,Location
    //   Top[S](TPlace.typ)

    /** Creates a link from an email */
    // case "link_email" => 
    //   val List(email_address) = parameters // String
    //   Top[S](TLink.typ)

    /** Creates a link from a phone number */
    // case "link_phone_number" => 
    //   val List(phone_number) = parameters // String
    //   Top[S](TLink.typ)

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
    //   Top[S](TMessage_Collection.typ)

    /** Searches for appointments in a given time range */
    case "search_appointments" =>
      val List(start,end) = parameters // DateTime,DateTime
      Top[S](TAppointment_Collection.typ)

    /** Searches for contacts by name. */
    // case "search_contacts" => 
    //   val List(prefix) = parameters // String
    //   Top[S](TContact_Collection.typ)

    /** Searches for places nearby. The distance is in meters. */
    case "search_places_nearby" =>
      val List(network,terms,location,distance) = parameters // String,String,Location,Number
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not, "search_places_nearby",
        "Check first if an internet connection is available")
      Top[S](TPlace_Collection.typ)

    /** Opens the mail client */
    case "send_email" =>
       val List(to,subject,body) = parameters // String,String,String
       // It's fine if we are offline here
       Skip

    /** Opens the short message client (to, body) */
    case "send_sms" =>
      val List(to,body) = parameters // String,String
      // TODO: Maybe check if length < 140?
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
