
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Place
 *
 * A named location
 *
 * @author Lucas Brutschy
 */ 

object TPlace {

  /** Gets the category of the place */
  val field_category = new TouchField("category",TString.typ)

  /** Gets the link associated to the message */
  val field_link = new TouchField("link",TString.typ)

  /** Gets the location of the place */
  val field_location = new TouchField("location",TLocation.typ)

  /** Gets the name of the place */
  val field_name = new TouchField("name",TString.typ)

  /** Gets a url to the picture */
  val field_picture_link = new TouchField("picture_link",TString.typ)

  /** Gets the source of this place (facebook, touchdevelop) */
  val field_source = new TouchField("source",TString.typ)

  val typName = "Place"
  val typ = new TouchType(typName,isSingleton = false,List(field_category, field_link, field_location,
    field_name, field_picture_link, field_source))

}

class TPlace extends AAny {

  def getTyp = TPlace.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Checks into the place (supported for Facebook) */
    case "check_in" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
