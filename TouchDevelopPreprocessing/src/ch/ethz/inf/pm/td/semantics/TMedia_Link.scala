
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Media Link
 *
 * A media file on the home network
 *
 * @author Lucas Brutschy
 */ 

object TMedia_Link {

  val typName = "Media Link"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TMedia_Link extends AAny {

  def getTyp = TMedia_Link.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the album if available */
    // case "album" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the album if available */
    //   val field_album = new TouchField("album",TString.typ)

    /** Gets the author if available */
    // case "author" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the author if available */
    //   val field_author = new TouchField("author",TString.typ)

    /** Gets the date if available */
    // case "date" => 
    //   Return[S](Valid(TDateTime.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the date if available */
    //   val field_date = new TouchField("date",TDateTime.typ)

    /** Gets the duration in seconds (0 for pictures) */
    // case "duration" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the duration in seconds (0 for pictures) */
    //   val field_duration = new TouchField("duration",TNumber.typ)

    /** Gets the kind of media (video, song, picture) */
    // case "kind" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the kind of media (video, song, picture) */
    //   val field_kind = new TouchField("kind",TString.typ)

    /** Plays or displays the media on the phone */
    // case "play" => 
    //   Skip;

    /** Gets the title if available */
    // case "title" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the title if available */
    //   val field_title = new TouchField("title",TString.typ)

    // FIELDS: , field_album, field_author, field_date, field_duration, field_kind, field_title

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
