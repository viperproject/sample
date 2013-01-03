
package ch.ethz.inf.pm.td.semantics

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

  val typName = "Link"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TLink extends AAny {

  def getTyp = TLink.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the url */
    // case "address" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the url */
    //   val field_address = new TouchField("address",TString.typ)

    /** Gets the kind of asset - media, image, email, phone number, hyperlink, deep zoom link, radio */
    // case "kind" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the kind of asset - media, image, email, phone number, hyperlink, deep zoom link, radio */
    //   val field_kind = new TouchField("kind",TString.typ)

    /** Gets the location if any */
    // case "location" => 
    //   Return[S](Valid(TLocation.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the location if any */
    //   val field_location = new TouchField("location",TLocation.typ)

    /** Gets the name if any */
    // case "name" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the name if any */
    //   val field_name = new TouchField("name",TString.typ)

    /** Sets the location */
    // case "set_location" => 
    //   val List(location) = parameters // Location
    //   Skip;

    /** Sets the name */
    // case "set_name" => 
    //   val List(name) = parameters // String
    //   Skip;

    /** Shares the link (email, sms, facebook, social or '' to pick from a list) */
    // case "share" => 
    //   val List(network) = parameters // String
    //   Skip;

    // FIELDS: , field_address, field_kind, field_location, field_name

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
