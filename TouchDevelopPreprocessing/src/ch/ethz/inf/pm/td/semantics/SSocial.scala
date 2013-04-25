
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
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

  val typName = "Social"
  val typ = new TouchType(typName,isSingleton = true)

}

class SSocial extends AAny {

  def getTyp = SSocial.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Chooses a contact from the contact list */
    // case "choose contact" =>
    //   Top[S](TContact.typ)
    // DECLARATION AS FIELD: 
    //   /** Chooses a contact from the contact list */
    //   val field_choose_contact = new TouchField("choose contact",TContact.typ)

    /** Chooses an email from the contact list */
    // case "choose email" =>
    //   Top[S](TLink.typ)
    // DECLARATION AS FIELD: 
    //   /** Chooses an email from the contact list */
    //   val field_choose_email = new TouchField("choose email",TLink.typ)

    /** Creates a new contact */
    // case "create contact" =>
    //   val List(nickname) = parameters // String
    //   Top[S](TContact.typ)

    /** Creates a message to share */
    case "create message" =>
      val List(message) = parameters // String
      New[S](TMessage.typ,Map(
        TMessage.field_message -> message
      ))

    /** Creates a place */
    case "create place" =>
       val List(name,location) = parameters // String,Location
       New[S](TPlace.typ,Map(
         TPlace.field_name -> name,
         TPlace.field_location -> location
       ))

    /** Creates a link from an email */
    case "link email" =>
      val List(email_address) = parameters // String
      New[S](TLink.typ,Map(
         TLink.field_address -> email_address,
         TLink.field_kind -> String("email")
      ))

    /** Creates a link from a phone number */
    case "link phone number" =>
      val List(phone_number) = parameters // String
      New[S](TLink.typ,Map(
        TLink.field_address -> phone_number,
        TLink.field_kind -> String("phone number")
      ))

    /** Saves a new contact */
    // case "save contact" =>
    //  val List(contact) = parameters // Contact
    //  Skip

    /** Allows the user to save the email address (email) */
    // case "save email" =>
    //   val List(email_address) = parameters // String
    //   Skip;

    /** Searches for recent messages in a social network (twitter, facebook) */
    // case "search" => 
    //   val List(network,terms) = parameters // String,String
    //   Top[S](TMessage_Collection.typ)

    /** Searches for appointments in a given time range */
    case "search appointments" =>
      val List(start,end) = parameters // DateTime,DateTime
      Top[S](TAppointment_Collection.typ)

    /** Searches for contacts by name. */
    // case "search contacts" =>
    //   val List(prefix) = parameters // String
    //   Top[S](TContact_Collection.typ)

    /** Searches for places nearby. The distance is in meters. */
    case "search places nearby" =>
      val List(network,terms,location,distance) = parameters // String,String,Location,Number
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not, "search places nearby",
        "Check first if an internet connection is available")
      Top[S](TPlace_Collection.typ)

    /** Opens the mail client */
    case "send email" =>
       val List(to,subject,body) = parameters // String,String,String
       // It's fine if we are offline here
       Skip

    /** Opens the short message client (to, body) */
    case "send sms" =>
      val List(to,body) = parameters // String,String
      // TODO: Maybe check if length < 140?
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
