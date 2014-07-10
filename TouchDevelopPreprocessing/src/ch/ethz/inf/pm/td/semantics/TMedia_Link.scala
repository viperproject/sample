
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._

/**
 * Specifies the abstract semantics of Media Link
 *
 * A media file on the home network
 *
 * @author Lucas Brutschy
 */

object TMedia_Link {

  /** Gets the album if available */
  val field_album = new TouchField("album", TString.typName, TopWithInvalidInitializer("link may not have a album"))

  /** Gets the author if available */
  val field_author = new TouchField("author", TString.typName, TopWithInvalidInitializer("link may not have an author"))

  /** Gets the date if available */
  val field_date = new TouchField("date", TDateTime.typName, TopWithInvalidInitializer("link may not have a date"))

  /** Gets the duration in seconds (0 for pictures) */
  val field_duration = new TouchField("duration", TNumber.typName, TopInitializer)

  /** Gets the kind of media (video, song, picture) */
  val field_kind = new TouchField("kind", TString.typName, TopInitializer)

  /** Gets the title if available */
  val field_title = new TouchField("title", TString.typName, TopWithInvalidInitializer("link may not have a title"))

  val typName = "Media Link"
  val typ = DefaultTouchType(typName, isSingleton = false, fields = List(field_album, field_author, field_date, field_duration, field_kind, field_title))

}

class TMedia_Link extends AAny {

  def getTyp = TMedia_Link.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Plays or displays the media on the phone */
    case "play" =>
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
