package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
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
  val field_birthday = new TouchField("birthday",TDateTime.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the company name if any. */
  val field_company = new TouchField("company",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the work or personal email if any */
  val field_email = new TouchField("email",TLink.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the first name if any. */
  val field_first_name = new TouchField("first name",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the work address if any */
  val field_home_address = new TouchField("home address",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the home phone number if any */
  val field_home_phone = new TouchField("home phone",TLink.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the job title at the company if any. */
  val field_job_title = new TouchField("job title",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the last name if any. */
  val field_last_name = new TouchField("last name",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the middle name if any. */
  val field_middle_name = new TouchField("middle name",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the cell phone number if any */
  val field_mobile_phone = new TouchField("mobile phone",TLink.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the display name (not used when saving contact) */
  val field_name = new TouchField("name",TString.typName)

  /** Gets the nickname if any. */
  val field_nick_name = new TouchField("nick name",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the office location at the company if any. */
  val field_office = new TouchField("office",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the personal email if any */
  val field_personal_email = new TouchField("personal email",TLink.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the cell or work or home phone number if any */
  val field_phone_number = new TouchField("phone number",TLink.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the picture of the contact if any. */
  val field_picture = new TouchField("picture",TPicture.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the source of this contact (phone, etc...) */
  val field_source = new TouchField("source",TString.typName)

  /** Gets the name suffix if any. */
  val field_suffix = new TouchField("suffix",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the name title if any. */
  val field_title = new TouchField("title",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the web site if any */
  val field_web_site = new TouchField("web site",TLink.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the home address if any */
  val field_work_address = new TouchField("work address",TString.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the work email if any */
  val field_work_email = new TouchField("work email",TLink.typName,topDefault = TopWithInvalidInitializer)

  /** Gets the work phone number if any */
  val field_work_phone = new TouchField("work phone",TLink.typName,topDefault = TopWithInvalidInitializer)

  val typName = "Contact"
  val typ = DefaultTouchType(typName,isSingleton = false, fields = List(
    field_birthday, field_company, field_email, field_first_name, field_home_address, field_home_phone, field_job_title,
    field_last_name, field_middle_name, field_mobile_phone, field_name, field_nick_name, field_office,
    field_personal_email, field_phone_number, field_picture, field_source, field_suffix, field_title, field_web_site,
    field_work_address, field_work_email, field_work_phone
  ))

}

class TContact extends AAny {

  def getTyp = TContact.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}