
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
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

  /** Gets the message identifier */
  val field_id = new TouchField("id",TString.typName)

  /** Gets the author */
  val field_from = new TouchField("from",TString.typName)

  /** Gets the link associated to the message */
  val field_link = new TouchField("link",TString.typName)

  /** Gets the geo coordinates */
  val field_location = new TouchField("location",TLocation.typName)

  /** Gets a url to the media */
  val field_media_link = new TouchField("media link",TString.typName)

  /** Gets the message text */
  val field_message = new TouchField("message",TString.typName)

  /** Gets a url to the picture */
  val field_picture_link = new TouchField("picture link",TString.typName)

  /** Gets the source of this message (Facebook, Twitter, etc...) */
  val field_source = new TouchField("source",TString.typName)

  /** Gets the time */
  val field_time = new TouchField("time",TDateTime.typName)

  /** Gets the title text */
  val field_title = new TouchField("title",TString.typName)

  /** Gets the recipient */
  val field_to = new TouchField("to",TString.typName)

  /** Gets the additional values stored in the message */
  val field_values = new TouchField("values",TString_Map.typName)

  val typName = "Message"
  val typ = new TouchType(typName,isSingleton = false, fields = List(field_from, field_link, field_location, field_media_link,
    field_message, field_picture_link, field_source, field_time, field_title, field_to, field_values, field_id), isImmutable = false)

}

class TMessage extends AAny {

  def getTyp = TMessage.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Shares this message (email, sms, facebook, social or '' to pick from a list) */
    case "share" =>
       val List(where) = parameters // String
       // TODO: Check for valid value
       Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
