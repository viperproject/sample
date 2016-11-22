/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Contact
 *
 * A contact
 *
 * @author Lucas Brutschy
 */

trait Default_TContact extends AAny {

  lazy val typeName = TypeName("Contact")
          
  /** Never used: Gets the birth date if any. */
  def member_birthday = ApiMember(
    name = "birthday",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the company name if any. */
  def member_company = ApiMember(
    name = "company",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the work or personal email if any */
  def member_email = ApiMember(
    name = "email",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the first name if any. */
  def member_first_name = ApiMember(
    name = "first name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the work address if any */
  def member_home_address = ApiMember(
    name = "home address",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the home phone number if any */
  def member_home_phone = ApiMember(
    name = "home phone",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the user id if any */
  def member_id = ApiMember(
    name = "id",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the job title at the company if any. */
  def member_job_title = ApiMember(
    name = "job title",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the last name if any. */
  def member_last_name = ApiMember(
    name = "last name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the middle name if any. */
  def member_middle_name = ApiMember(
    name = "middle name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the cell phone number if any */
  def member_mobile_phone = ApiMember(
    name = "mobile phone",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the display name (not used when saving contact) */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the nickname if any. */
  def member_nick_name = ApiMember(
    name = "nick name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the office location at the company if any. */
  def member_office = ApiMember(
    name = "office",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the personal email if any */
  def member_personal_email = ApiMember(
    name = "personal email",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the cell or work or home phone number if any */
  def member_phone_number = ApiMember(
    name = "phone number",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the picture of the contact if any. */
  def member_picture = ApiMember(
    name = "picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the picture url */
  def member_setPicture_url = ApiMember(
    name = "set Picture url",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the birthday */
  def member_set_birthday = ApiMember(
    name = "set birthday",
    paramTypes = List(ApiParam(TDateTime)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the company */
  def member_set_company = ApiMember(
    name = "set company",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the work or personal email */
  def member_set_email = ApiMember(
    name = "set email",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the first name */
  def member_set_first_name = ApiMember(
    name = "set first name",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the home address */
  def member_set_home_address = ApiMember(
    name = "set home address",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the home phone */
  def member_set_home_phone = ApiMember(
    name = "set home phone",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the user id */
  def member_set_id = ApiMember(
    name = "set id",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the job title */
  def member_set_job_title = ApiMember(
    name = "set job title",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the last name */
  def member_set_last_name = ApiMember(
    name = "set last name",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the middle name */
  def member_set_middle_name = ApiMember(
    name = "set middle name",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the mobile phone */
  def member_set_mobile_phone = ApiMember(
    name = "set mobile phone",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the middle name */
  def member_set_nick_name = ApiMember(
    name = "set nick name",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the office location at the company */
  def member_set_office = ApiMember(
    name = "set office",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the personal email */
  def member_set_personal_email = ApiMember(
    name = "set personal email",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the cell or work or home phone number if any */
  def member_set_phone_number = ApiMember(
    name = "set phone number",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the picture */
  def member_set_picture = ApiMember(
    name = "set picture",
    paramTypes = List(ApiParam(TPicture)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the source */
  def member_set_source = ApiMember(
    name = "set source",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the suffix */
  def member_set_suffix = ApiMember(
    name = "set suffix",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the title */
  def member_set_title = ApiMember(
    name = "set title",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the web site */
  def member_set_web_site = ApiMember(
    name = "set web site",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the work address */
  def member_set_work_address = ApiMember(
    name = "set work address",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the work email */
  def member_set_work_email = ApiMember(
    name = "set work email",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the work phone */
  def member_set_work_phone = ApiMember(
    name = "set work phone",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the source of this contact (phone, etc...) */
  def member_source = ApiMember(
    name = "source",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the name suffix if any. */
  def member_suffix = ApiMember(
    name = "suffix",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the name title if any. */
  def member_title = ApiMember(
    name = "title",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the web site if any */
  def member_web_site = ApiMember(
    name = "web site",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the home address if any */
  def member_work_address = ApiMember(
    name = "work address",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the work email if any */
  def member_work_email = ApiMember(
    name = "work email",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the work phone number if any */
  def member_work_phone = ApiMember(
    name = "work phone",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "birthday" -> member_birthday,
    "company" -> member_company,
    "email" -> member_email,
    "first name" -> member_first_name,
    "home address" -> member_home_address,
    "home phone" -> member_home_phone,
    "id" -> member_id,
    "job title" -> member_job_title,
    "last name" -> member_last_name,
    "middle name" -> member_middle_name,
    "mobile phone" -> member_mobile_phone,
    "name" -> member_name,
    "nick name" -> member_nick_name,
    "office" -> member_office,
    "personal email" -> member_personal_email,
    "phone number" -> member_phone_number,
    "picture" -> member_picture,
    "set Picture url" -> member_setPicture_url,
    "set birthday" -> member_set_birthday,
    "set company" -> member_set_company,
    "set email" -> member_set_email,
    "set first name" -> member_set_first_name,
    "set home address" -> member_set_home_address,
    "set home phone" -> member_set_home_phone,
    "set id" -> member_set_id,
    "set job title" -> member_set_job_title,
    "set last name" -> member_set_last_name,
    "set middle name" -> member_set_middle_name,
    "set mobile phone" -> member_set_mobile_phone,
    "set nick name" -> member_set_nick_name,
    "set office" -> member_set_office,
    "set personal email" -> member_set_personal_email,
    "set phone number" -> member_set_phone_number,
    "set picture" -> member_set_picture,
    "set source" -> member_set_source,
    "set suffix" -> member_set_suffix,
    "set title" -> member_set_title,
    "set web site" -> member_set_web_site,
    "set work address" -> member_set_work_address,
    "set work email" -> member_set_work_email,
    "set work phone" -> member_set_work_phone,
    "source" -> member_source,
    "suffix" -> member_suffix,
    "title" -> member_title,
    "web site" -> member_web_site,
    "work address" -> member_work_address,
    "work email" -> member_work_email,
    "work phone" -> member_work_phone
  )
            

}
          
