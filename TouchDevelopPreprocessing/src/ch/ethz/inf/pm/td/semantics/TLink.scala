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
import ch.ethz.inf.pm.td.analysis.{InvalidInitializer, ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TLink
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Link
 *
 * A link to a video, image, email, phone number
 *
 * @author Lucas Brutschy
 */

object TLink extends Default_TLink {

  /** Gets the url */
  lazy val field_address = ApiField("address", TString)

  /** Gets the kind of asset - media, image, email, phone number, hyperlink, deep zoom link, radio */
  lazy val field_kind = ApiField("kind", TString)

  /** Gets the location if any */
  lazy val field_location = ApiField("location", TLocation, InvalidInitializer("link may not have a location"))

  /** Gets the name if any */
  lazy val field_name = ApiField("name", TString, InvalidInitializer("link may not have a name"))

  override def possibleFields = super.possibleFields ++ List(field_address, field_kind, field_location, field_name)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Shares the link (email, sms, facebook, social or empty to pick from a list) */
    case "share" =>
      val List(network) = parameters // String
      // TODO: Check range of parameters
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
