/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TopWithInvalidInitializer, ApiField}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TContact
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

object TContact extends Default_TContact {

  /** Gets the birth date if any. */
  lazy val field_birthday = ApiField("birthday", TDateTime, topDefault = TopWithInvalidInitializer("contact may not have a birthday"))

  /** Gets the company name if any. */
  lazy val field_company = ApiField("company", TString, topDefault = TopWithInvalidInitializer("contact may not have a company"))

  /** Gets the work or personal email if any */
  lazy val field_email = ApiField("email", TLink, topDefault = TopWithInvalidInitializer("contact may not have an email"))

  /** Gets the first name if any. */
  lazy val field_first_name = ApiField("first name", TString, topDefault = TopWithInvalidInitializer("contact may not have a first name"))

  /** Gets the work address if any */
  lazy val field_home_address = ApiField("home address", TString, topDefault = TopWithInvalidInitializer("contact may not have a home address"))

  /** Gets the home phone number if any */
  lazy val field_home_phone = ApiField("home phone", TLink, topDefault = TopWithInvalidInitializer("contact may not have a home phone"))

  /** Gets the job title at the company if any. */
  lazy val field_job_title = ApiField("job title", TString, topDefault = TopWithInvalidInitializer("contact may not have a job title"))

  /** Gets the last name if any. */
  lazy val field_last_name = ApiField("last name", TString, topDefault = TopWithInvalidInitializer("contact may not have a last name"))

  /** Gets the middle name if any. */
  lazy val field_middle_name = ApiField("middle name", TString, topDefault = TopWithInvalidInitializer("contact may not have a middle name"))

  /** Gets the cell phone number if any */
  lazy val field_mobile_phone = ApiField("mobile phone", TLink, topDefault = TopWithInvalidInitializer("contact may not have a mobile phone"))

  /** Gets the display name (not used when saving contact) */
  lazy val field_name = ApiField("name", TString)

  /** Gets the nickname if any. */
  lazy val field_nick_name = ApiField("nick name", TString, topDefault = TopWithInvalidInitializer("contact may not have a nick name"))

  /** Gets the office location at the company if any. */
  lazy val field_office = ApiField("office", TString, topDefault = TopWithInvalidInitializer("contact may not have an office"))

  /** Gets the personal email if any */
  lazy val field_personal_email = ApiField("personal email", TLink, topDefault = TopWithInvalidInitializer("contact may not have a personal email"))

  /** Gets the cell or work or home phone number if any */
  lazy val field_phone_number = ApiField("phone number", TLink, topDefault = TopWithInvalidInitializer("contact may not have a phone number"))

  /** Gets the picture of the contact if any. */
  lazy val field_picture = ApiField("picture", TPicture, topDefault = TopWithInvalidInitializer("contact may not have a picture"))

  /** Gets the source of this contact (phone, etc...) */
  lazy val field_source = ApiField("source", TString)

  /** Gets the name suffix if any. */
  lazy val field_suffix = ApiField("suffix", TString, topDefault = TopWithInvalidInitializer("contact may not have a suffix"))

  /** Gets the name title if any. */
  lazy val field_title = ApiField("title", TString, topDefault = TopWithInvalidInitializer("contact may not have a title"))

  /** Gets the web site if any */
  lazy val field_web_site = ApiField("web site", TLink, topDefault = TopWithInvalidInitializer("contact may not have a website"))

  /** Gets the home address if any */
  lazy val field_work_address = ApiField("work address", TString, topDefault = TopWithInvalidInitializer("contact may not have a work address"))

  /** Gets the work email if any */
  lazy val field_work_email = ApiField("work email", TLink, topDefault = TopWithInvalidInitializer("contact may not have a work email"))

  /** Gets the work phone number if any */
  lazy val field_work_phone = ApiField("work phone", TLink, topDefault = TopWithInvalidInitializer("contact may not have a work phone"))

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