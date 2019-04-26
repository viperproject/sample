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
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TMessage
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Message
 *
 * A post on a message board
 *
 * @author Lucas Brutschy
 */ 

object TMessage extends Default_TMessage {

  /** Gets the message identifier */
  lazy val field_id = ApiField("id", TString)

  /** Gets the author */
  lazy val field_from = ApiField("from", TString)

  /** Gets the link associated to the message */
  lazy val field_link = ApiField("link", TString)

  /** Gets the geo coordinates */
  lazy val field_location = ApiField("location", TLocation)

  /** Gets a url to the media */
  lazy val field_media_link = ApiField("media link", TString)

  /** Gets the message text */
  lazy val field_message = ApiField("message", TString)

  /** Gets a url to the picture */
  lazy val field_picture_link = ApiField("picture link", TString)

  /** Gets the source of this message (Facebook, Twitter, etc...) */
  lazy val field_source = ApiField("source", TString)

  /** Gets the time */
  lazy val field_time = ApiField("time", TDateTime)

  /** Gets the title text */
  lazy val field_title = ApiField("title", TString)

  /** Gets the recipient */
  lazy val field_to = ApiField("to", TString)

  /** Gets the additional values stored in the message */
  lazy val field_values = ApiField("values", TString_Map)

  override def possibleFields = super.possibleFields ++ List(field_from, field_link, field_location, field_media_link,
    field_message, field_picture_link, field_source, field_time, field_title, field_to, field_values, field_id)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Shares this message (email, sms, facebook, social or &#39;&#39; to pick from a list) */
    case "share" =>
       val List(where) = parameters // String
       // TODO: Check for valid value
       Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
