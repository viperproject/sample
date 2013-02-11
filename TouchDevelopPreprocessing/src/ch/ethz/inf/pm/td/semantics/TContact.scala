package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Contact
 *
 * A contact
 *
 * TODO: Almost everything should default to probably invalid here
 *
 * @author Lucas Brutschy
 */

object TContact {

  /** Gets the birth date if any. */
  val field_birthday = new TouchField("birthday",TDateTime.typ)

  /** Gets the company name if any. */
  val field_company = new TouchField("company",TString.typ)

  /** Gets the work or personal email if any */
  val field_email = new TouchField("email",TLink.typ)

  /** Gets the first name if any. */
  val field_first_name = new TouchField("first_name",TString.typ)

  /** Gets the work address if any */
  val field_home_address = new TouchField("home_address",TString.typ)

  /** Gets the home phone number if any */
  val field_home_phone = new TouchField("home_phone",TLink.typ)

  /** Gets the job title at the company if any. */
  val field_job_title = new TouchField("job_title",TString.typ)

  /** Gets the last name if any. */
  val field_last_name = new TouchField("last_name",TString.typ)

  /** Gets the middle name if any. */
  val field_middle_name = new TouchField("middle_name",TString.typ)

  /** Gets the cell phone number if any */
  val field_mobile_phone = new TouchField("mobile_phone",TLink.typ)

  /** Gets the display name (not used when saving contact) */
  val field_name = new TouchField("name",TString.typ)

  /** Gets the nickname if any. */
  val field_nick_name = new TouchField("nick_name",TString.typ)

  /** Gets the office location at the company if any. */
  val field_office = new TouchField("office",TString.typ)

  /** Gets the personal email if any */
  val field_personal_email = new TouchField("personal_email",TLink.typ)

  /** Gets the cell or work or home phone number if any */
  val field_phone_number = new TouchField("phone_number",TLink.typ)

  /** Gets the picture of the contact if any. */
  val field_picture = new TouchField("picture",TPicture.typ)

  /** Gets the source of this contact (phone, etc...) */
  val field_source = new TouchField("source",TString.typ)

  /** Gets the name suffix if any. */
  val field_suffix = new TouchField("suffix",TString.typ)

  /** Gets the name title if any. */
  val field_title = new TouchField("title",TString.typ)

  /** Gets the web site if any */
  val field_web_site = new TouchField("web_site",TLink.typ)

  /** Gets the home address if any */
  val field_work_address = new TouchField("work_address",TString.typ)

  /** Gets the work email if any */
  val field_work_email = new TouchField("work_email",TLink.typ)

  /** Gets the work phone number if any */
  val field_work_phone = new TouchField("work_phone",TLink.typ)

  val typName = "Contact"
  val typ = new TouchType(typName,isSingleton = false,List(
    field_birthday, field_company, field_email, field_first_name, field_home_address, field_home_phone, field_job_title,
    field_last_name, field_middle_name, field_mobile_phone, field_name, field_nick_name, field_office,
    field_personal_email, field_phone_number, field_picture, field_source, field_suffix, field_title, field_web_site,
    field_work_address, field_work_email, field_work_phone
  ))

}

class TContact extends AAny {

  def getTyp = TContact.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}