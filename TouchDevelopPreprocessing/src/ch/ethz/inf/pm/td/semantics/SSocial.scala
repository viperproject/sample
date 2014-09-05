
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of social
 *
 * Emails, sms, contacts, calendar, ...
 *
 * @author Lucas Brutschy
 */

object SSocial extends ASingleton {

  lazy val typeName = TypeName("Social")

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Chooses a contact from the contact list */
    case "choose contact" =>
      TopWithInvalid[S](TContact, "user may cancel contact selection") // Invalid value validated in Windows Phone version!

    /** Chooses an email from the contact list */
    case "choose email" =>
      TopWithInvalid[S](TLink, "user may cancel email selection") // Invalid value validated in Windows Phone version!

    /** Retrieves the list of contacts */
    case "contacts" =>
      val List(network) = parameters // String
      TopWithInvalid[S](TContact_Collection, "device may not have a contacts library")

    /** Creates a new contact */
    case "create contact" =>
      val List(nickname) = parameters // String
      New[S](TContact, Map(TContact.field_nick_name -> nickname))

    /** Creates a message to share */
    case "create message" =>
      val List(message) = parameters // String
      New[S](TMessage, Map(
        TMessage.field_message -> message
      ))

    /** Creates a place */
    case "create place" =>
      val List(name, location) = parameters // String,Location
      New[S](TPlace, Map(
        TPlace.field_name -> name,
        TPlace.field_location -> location
      ))

    /** Creates a link from an email */
    case "link email" =>
      val List(email_address) = parameters // String
      New[S](TLink, Map(
        TLink.field_address -> email_address,
        TLink.field_kind -> String("email")
      ))

    /** Creates a link from a phone number */
    case "link phone number" =>
      val List(phone_number) = parameters // String
      New[S](TLink, Map(
        TLink.field_address -> phone_number,
        TLink.field_kind -> String("phone number")
      ))

    /** Saves a new contact */
    case "save contact" =>
      val List(contact) = parameters // Contact
      Skip

    /** Allows the user to save the email address (email) */
    case "save email" =>
      val List(email_address) = parameters // String
      Skip

    /** Searches for recent messages in a social network (twitter, facebook) */
    case "search" =>
      val List(network, terms) = parameters // String,String
      TopWithInvalid[S](TMessage_Collection, "social network may not be reachable")

    /** Searches for appointments in a given time range */
    case "search appointments" =>
      val List(start, end) = parameters // DateTime,DateTime
      Top[S](TAppointment_Collection)

    /** Searches for contacts by name. */
    case "search contacts" =>
      val List(prefix) = parameters // String
      Top[S](TContact_Collection)

    /** Searches for places nearby. The distance is in meters. */
    case "search places nearby" =>
      val List(network, terms, location, distance) = parameters // String,String,Location,Number
      Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not, "search places nearby",
        "Check first if an internet connection is available")
      Top[S](TPlace_Collection)

    /** Opens the mail client */
    case "send email" =>
      val List(to, subject, body) = parameters // String,String,String
      // It's fine if we are offline here
      Skip

    /** Opens the short message client (to, body) */
    case "send sms" =>
      val List(to, body) = parameters // String,String
      // TODO: Maybe check if length < 140?
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
