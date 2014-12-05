
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Social
 *
 * Emails, sms, contacts, calendar, ...
 *
 * @author Lucas Brutschy
 */

trait Default_SSocial extends ASingleton {

  lazy val typeName = TypeName("Social")
          
  /** Rarely used: Chooses a contact from the contact list */
  def member_choose_contact = ApiMember(
    name = "choose contact",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TContact,
    semantics = DefaultSemantics
  )

  /** Rarely used: Chooses an email from the contact list */
  def member_choose_email = ApiMember(
    name = "choose email",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Rarely used: Retrieves the list of contacts */
  def member_contacts = ApiMember(
    name = "contacts",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TContact_Collection,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a new contact */
  def member_create_contact = ApiMember(
    name = "create contact",
    paramTypes = List(ApiParam(TString,isMutated=true)),
    thisType = ApiParam(this),
    returnType = TContact,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a message to share */
  def member_create_message = ApiMember(
    name = "create message",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TMessage,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a place */
  def member_create_place = ApiMember(
    name = "create place",
    paramTypes = List(ApiParam(TString), ApiParam(TLocation)),
    thisType = ApiParam(this),
    returnType = TPlace,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a link from an email */
  def member_link_email = ApiMember(
    name = "link email",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a link from a phone number */
  def member_link_phone_number = ApiMember(
    name = "link phone number",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Never used: Saves a new contact */
  def member_save_contact = ApiMember(
    name = "save contact",
    paramTypes = List(ApiParam(TContact)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Allows the user to save the email address (email) */
  def member_save_email = ApiMember(
    name = "save email",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Searches for appointments in a given time range */
  def member_search_appointments = ApiMember(
    name = "search appointments",
    paramTypes = List(ApiParam(TDateTime), ApiParam(TDateTime)),
    thisType = ApiParam(this),
    returnType = TAppointment_Collection,
    semantics = DefaultSemantics
  )

  /** Rarely used: Searches for contacts by name. */
  def member_search_contacts = ApiMember(
    name = "search contacts",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TContact_Collection,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] [**obsolete**] Searches for places nearby. The distance is in meters. */
  def member_search_places_nearby = ApiMember(
    name = "search places nearby",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TLocation), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TPlace_Collection,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Searches for recent messages in a social network (twitter, facebook) */
  def member_search = ApiMember(
    name = "search",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TMessage_Collection,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Opens the mail client */
  def member_send_email = ApiMember(
    name = "send email",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Opens the short message client (to, body) */
  def member_send_sms = ApiMember(
    name = "send sms",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "choose contact" -> member_choose_contact,
    "choose email" -> member_choose_email,
    "contacts" -> member_contacts,
    "create contact" -> member_create_contact,
    "create message" -> member_create_message,
    "create place" -> member_create_place,
    "link email" -> member_link_email,
    "link phone number" -> member_link_phone_number,
    "save contact" -> member_save_contact,
    "save email" -> member_save_email,
    "search appointments" -> member_search_appointments,
    "search contacts" -> member_search_contacts,
    "search places nearby" -> member_search_places_nearby,
    "search" -> member_search,
    "send email" -> member_send_email,
    "send sms" -> member_send_sms
  )
            

}
          
