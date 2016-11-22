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
 * Specifies the abstract semantics of Message
 *
 * A post on a message board
 *
 * @author Lucas Brutschy
 */

trait Default_TMessage extends AAny {

  lazy val typeName = TypeName("Message")
          
  /** Rarely used: Gets the author */
  def member_from = ApiMember(
    name = "from",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the message identifier */
  def member_id = ApiMember(
    name = "id",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the link associated to the message */
  def member_link = ApiMember(
    name = "link",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the geo coordinates */
  def member_location = ApiMember(
    name = "location",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a url to the media */
  def member_media_link = ApiMember(
    name = "media link",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the message text */
  def member_message = ApiMember(
    name = "message",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets a url to the picture */
  def member_picture_link = ApiMember(
    name = "picture link",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the author */
  def member_set_from = ApiMember(
    name = "set from",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the message identifier */
  def member_set_id = ApiMember(
    name = "set id",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the link associated to the message */
  def member_set_link = ApiMember(
    name = "set link",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the geo coordinates */
  def member_set_location = ApiMember(
    name = "set location",
    paramTypes = List(ApiParam(TLocation)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the url to the media */
  def member_set_media_link = ApiMember(
    name = "set media link",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the message text */
  def member_set_message = ApiMember(
    name = "set message",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the url to the picture */
  def member_set_picture_link = ApiMember(
    name = "set picture link",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the source of this message */
  def member_set_source = ApiMember(
    name = "set source",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the time */
  def member_set_time = ApiMember(
    name = "set time",
    paramTypes = List(ApiParam(TDateTime)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the title text */
  def member_set_title = ApiMember(
    name = "set title",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the recipient */
  def member_set_to = ApiMember(
    name = "set to",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Shares this message (email, sms, facebook, social or &#39;&#39; to pick from a list) */
  def member_share = ApiMember(
    name = "share",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the source of this message (Facebook, Twitter, etc...) */
  def member_source = ApiMember(
    name = "source",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the time */
  def member_time = ApiMember(
    name = "time",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the title text */
  def member_title = ApiMember(
    name = "title",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the recipient */
  def member_to = ApiMember(
    name = "to",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the additional values stored in the message */
  def member_values = ApiMember(
    name = "values",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString_Map,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "from" -> member_from,
    "id" -> member_id,
    "link" -> member_link,
    "location" -> member_location,
    "media link" -> member_media_link,
    "message" -> member_message,
    "picture link" -> member_picture_link,
    "set from" -> member_set_from,
    "set id" -> member_set_id,
    "set link" -> member_set_link,
    "set location" -> member_set_location,
    "set media link" -> member_set_media_link,
    "set message" -> member_set_message,
    "set picture link" -> member_set_picture_link,
    "set source" -> member_set_source,
    "set time" -> member_set_time,
    "set title" -> member_set_title,
    "set to" -> member_set_to,
    "share" -> member_share,
    "source" -> member_source,
    "time" -> member_time,
    "title" -> member_title,
    "to" -> member_to,
    "values" -> member_values
  )
            

}
          
