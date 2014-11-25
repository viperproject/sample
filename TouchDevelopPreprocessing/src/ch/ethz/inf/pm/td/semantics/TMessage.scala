
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Message
 *
 * A post on a message board
 *
 * @author Lucas Brutschy
 */ 

object TMessage extends AAny {

  /** Gets the message identifier */
  lazy val field_id = new ApiField("id",TString.typeName)

  /** Gets the author */
  lazy val field_from = new ApiField("from",TString.typeName)

  /** Gets the link associated to the message */
  lazy val field_link = new ApiField("link",TString.typeName)

  /** Gets the geo coordinates */
  lazy val field_location = new ApiField("location",TLocation.typeName)

  /** Gets a url to the media */
  lazy val field_media_link = new ApiField("media link",TString.typeName)

  /** Gets the message text */
  lazy val field_message = new ApiField("message",TString.typeName)

  /** Gets a url to the picture */
  lazy val field_picture_link = new ApiField("picture link",TString.typeName)

  /** Gets the source of this message (Facebook, Twitter, etc...) */
  lazy val field_source = new ApiField("source",TString.typeName)

  /** Gets the time */
  lazy val field_time = new ApiField("time",TDateTime.typeName)

  /** Gets the title text */
  lazy val field_title = new ApiField("title",TString.typeName)

  /** Gets the recipient */
  lazy val field_to = new ApiField("to",TString.typeName)

  /** Gets the additional values stored in the message */
  lazy val field_values = new ApiField("values",TString_Map.typeName)

  lazy val typeName = TypeName("Message")

  override def possibleFields = super.possibleFields ++ List(field_from, field_link, field_location, field_media_link,
    field_message, field_picture_link, field_source, field_time, field_title, field_to, field_values, field_id)

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
      
