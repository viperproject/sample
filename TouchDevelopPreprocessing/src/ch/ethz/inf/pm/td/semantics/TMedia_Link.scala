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
import ch.ethz.inf.pm.td.analysis.{TopWithInvalidInitializer, TopInitializer, ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TMedia_Link
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Media Link
 *
 * A media file on the home network
 *
 * @author Lucas Brutschy
 */

object TMedia_Link extends Default_TMedia_Link {

  /** Gets the album if available */
  lazy val field_album = ApiField("album", TString, TopWithInvalidInitializer("link may not have a album"))

  /** Gets the author if available */
  lazy val field_author = ApiField("author", TString, TopWithInvalidInitializer("link may not have an author"))

  /** Gets the date if available */
  lazy val field_date = ApiField("date", TDateTime, TopWithInvalidInitializer("link may not have a date"))

  /** Gets the duration in seconds (0 for pictures) */
  lazy val field_duration = ApiField("duration", TNumber, TopInitializer)

  /** Gets the kind of media (video, song, picture) */
  lazy val field_kind = ApiField("kind", TString, TopInitializer)

  /** Gets the title if available */
  lazy val field_title = ApiField("title", TString, TopWithInvalidInitializer("link may not have a title"))

  override def possibleFields = super.possibleFields ++ List(field_album, field_author, field_date, field_duration,
    field_kind, field_title)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Plays or displays the media on the phone */
    case "play" =>
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
