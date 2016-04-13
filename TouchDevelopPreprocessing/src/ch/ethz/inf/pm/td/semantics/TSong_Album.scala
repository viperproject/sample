
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TSong_Album
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Song Album
 *
 * A song album
 *
 * @author Lucas Brutschy
 */ 

object TSong_Album extends Default_TSong_Album {

/** Gets album art picture */
  lazy val field_art = ApiField("art", TPicture)

  /** Gets the name of the artist */
  lazy val field_artist = ApiField("artist", TString)

  /** Gets the duration in seconds */
  lazy val field_duration = ApiField("duration", TNumber)

  /** Gets the genre of the song */
  lazy val field_genre = ApiField("genre", TString)

  /** Indicates if the album has art */
  lazy val field_has_art = ApiField("has art", TBoolean)

  /** Gets the name of the album */
  lazy val field_name = ApiField("name", TString)

  /** Gets the songs */
  lazy val field_songs = ApiField("songs", TSongs)

  /** Gets the thumbnail picture */
  lazy val field_thumbnail = ApiField("thumbnail", TPicture)

  override def possibleFields = super.possibleFields ++ List(field_art, field_artist, field_duration,
    field_genre, field_has_art, field_name, field_songs, field_thumbnail)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the songs of the album */
    case "play" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
