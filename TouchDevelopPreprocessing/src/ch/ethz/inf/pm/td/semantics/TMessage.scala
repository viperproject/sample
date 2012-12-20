
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Message
 *
 * A post on a message board
 *
 * @author Lucas Brutschy
 */ 

object TMessage {

  val typName = "Message"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TMessage extends AAny {

  def getTyp = TMessage.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the author */
    // case "from" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the author */
    //   val field_from = new TouchField("from",TString.typ)

    /** Gets the link associated to the message */
    // case "link" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the link associated to the message */
    //   val field_link = new TouchField("link",TString.typ)

    /** Gets the geo coordinates */
    // case "location" => 
    //   Return[S](Valid(TLocation.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the geo coordinates */
    //   val field_location = new TouchField("location",TLocation.typ)

    /** Gets a url to the media */
    // case "media_link" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets a url to the media */
    //   val field_media_link = new TouchField("media_link",TString.typ)

    /** Gets the message text */
    // case "message" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the message text */
    //   val field_message = new TouchField("message",TString.typ)

    /** Gets a url to the picture */
    // case "picture_link" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets a url to the picture */
    //   val field_picture_link = new TouchField("picture_link",TString.typ)

    /** Sets the author */
    // case "set_from" => 
    //   val List(author) = parameters // String
    //   Skip;

    /** Sets the link associated to the message */
    // case "set_link" => 
    //   val List(url) = parameters // String
    //   Skip;

    /** Sets the geo coordinates */
    // case "set_location" => 
    //   val List(location) = parameters // Location
    //   Skip;

    /** Sets the url to the media */
    // case "set_media_link" => 
    //   val List(url) = parameters // String
    //   Skip;

    /** Sets the message text */
    // case "set_message" => 
    //   val List(message) = parameters // String
    //   Skip;

    /** Sets the url to the picture */
    // case "set_picture_link" => 
    //   val List(url) = parameters // String
    //   Skip;

    /** Sets the source of this message */
    // case "set_source" => 
    //   val List(source) = parameters // String
    //   Skip;

    /** Sets the time */
    // case "set_time" => 
    //   val List(time) = parameters // DateTime
    //   Skip;

    /** Sets the title text */
    // case "set_title" => 
    //   val List(title) = parameters // String
    //   Skip;

    /** Sets the recipient */
    // case "set_to" => 
    //   val List(author) = parameters // String
    //   Skip;

    /** Shares this message (email, sms, facebook, social or '' to pick from a list) */
    // case "share" => 
    //   val List(where) = parameters // String
    //   Skip;

    /** Gets the source of this message (Facebook, Twitter, etc...) */
    // case "source" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the source of this message (Facebook, Twitter, etc...) */
    //   val field_source = new TouchField("source",TString.typ)

    /** Gets the time */
    // case "time" => 
    //   Return[S](Valid(TDateTime.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the time */
    //   val field_time = new TouchField("time",TDateTime.typ)

    /** Gets the title text */
    // case "title" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the title text */
    //   val field_title = new TouchField("title",TString.typ)

    /** Gets the recipient */
    // case "to" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the recipient */
    //   val field_to = new TouchField("to",TString.typ)

    /** Gets the additional values stored in the message */
    // case "values" => 
    //   Return[S](Valid(TString_Map.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the additional values stored in the message */
    //   val field_values = new TouchField("values",TString_Map.typ)

    // FIELDS: , field_from, field_link, field_location, field_media_link, field_message, field_picture_link, field_source, field_time, field_title, field_to, field_values

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
