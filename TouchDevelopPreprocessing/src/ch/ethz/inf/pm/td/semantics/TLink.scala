
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Link
 *
 * A link to a video, image, email, phone number
 *
 * @author Lucas Brutschy
 */ 

object TLink {

  /** Gets the url */
  val field_address = new TouchField("address",TString.typ)

  /** Gets the kind of asset - media, image, email, phone number, hyperlink, deep zoom link, radio */
  val field_kind = new TouchField("kind",TString.typ)

  /** Gets the location if any */
  val field_location = new TouchField("location",TLocation.typ,InvalidInitializer())

  /** Gets the name if any */
  val field_name = new TouchField("name",TString.typ,InvalidInitializer())

  val typName = "Link"
  val typ = new TouchType(typName,isSingleton = false,List(field_address, field_kind, field_location, field_name))

}

class TLink extends AAny {

  def getTyp = TLink.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Shares the link (email, sms, facebook, social or empty to pick from a list) */
    case "share" =>
      val List(network) = parameters // String
      // TODO: Check range of parameters
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
