package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TopWithInvalidInitializer, TouchField}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Contact
 *
 * A contact
 *
 * TODO: Almost everything should default to probably invalid here
 *
 * @author Lucas Brutschy
 */

object TContact extends AAny {

  /** Gets the birth date if any. */
  lazy val field_birthday = new TouchField("birthday", TDateTime.typeName, topDefault = TopWithInvalidInitializer("contact may not have a birthday"))

  /** Gets the company name if any. */
  lazy val field_company = new TouchField("company", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have a company"))

  /** Gets the work or personal email if any */
  lazy val field_email = new TouchField("email", TLink.typeName, topDefault = TopWithInvalidInitializer("contact may not have an email"))

  /** Gets the first name if any. */
  lazy val field_first_name = new TouchField("first name", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have a first name"))

  /** Gets the work address if any */
  lazy val field_home_address = new TouchField("home address", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have a home address"))

  /** Gets the home phone number if any */
  lazy val field_home_phone = new TouchField("home phone", TLink.typeName, topDefault = TopWithInvalidInitializer("contact may not have a home phone"))

  /** Gets the job title at the company if any. */
  lazy val field_job_title = new TouchField("job title", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have a job title"))

  /** Gets the last name if any. */
  lazy val field_last_name = new TouchField("last name", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have a last name"))

  /** Gets the middle name if any. */
  lazy val field_middle_name = new TouchField("middle name", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have a middle name"))

  /** Gets the cell phone number if any */
  lazy val field_mobile_phone = new TouchField("mobile phone", TLink.typeName, topDefault = TopWithInvalidInitializer("contact may not have a mobile phone"))

  /** Gets the display name (not used when saving contact) */
  lazy val field_name = new TouchField("name", TString.typeName)

  /** Gets the nickname if any. */
  lazy val field_nick_name = new TouchField("nick name", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have a nick name"))

  /** Gets the office location at the company if any. */
  lazy val field_office = new TouchField("office", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have an office"))

  /** Gets the personal email if any */
  lazy val field_personal_email = new TouchField("personal email", TLink.typeName, topDefault = TopWithInvalidInitializer("contact may not have a personal email"))

  /** Gets the cell or work or home phone number if any */
  lazy val field_phone_number = new TouchField("phone number", TLink.typeName, topDefault = TopWithInvalidInitializer("contact may not have a phone number"))

  /** Gets the picture of the contact if any. */
  lazy val field_picture = new TouchField("picture", TPicture.typeName, topDefault = TopWithInvalidInitializer("contact may not have a picture"))

  /** Gets the source of this contact (phone, etc...) */
  lazy val field_source = new TouchField("source", TString.typeName)

  /** Gets the name suffix if any. */
  lazy val field_suffix = new TouchField("suffix", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have a suffix"))

  /** Gets the name title if any. */
  lazy val field_title = new TouchField("title", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have a title"))

  /** Gets the web site if any */
  lazy val field_web_site = new TouchField("web site", TLink.typeName, topDefault = TopWithInvalidInitializer("contact may not have a website"))

  /** Gets the home address if any */
  lazy val field_work_address = new TouchField("work address", TString.typeName, topDefault = TopWithInvalidInitializer("contact may not have a work address"))

  /** Gets the work email if any */
  lazy val field_work_email = new TouchField("work email", TLink.typeName, topDefault = TopWithInvalidInitializer("contact may not have a work email"))

  /** Gets the work phone number if any */
  lazy val field_work_phone = new TouchField("work phone", TLink.typeName, topDefault = TopWithInvalidInitializer("contact may not have a work phone"))

  lazy val typeName = TypeName("Contact")

  override def possibleFields = super.possibleFields ++ List(
    field_birthday, field_company, field_email, field_first_name, field_home_address, field_home_phone, field_job_title,
    field_last_name, field_middle_name, field_mobile_phone, field_name, field_nick_name, field_office,
    field_personal_email, field_phone_number, field_picture, field_source, field_suffix, field_title, field_web_site,
    field_work_address, field_work_email, field_work_phone
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}