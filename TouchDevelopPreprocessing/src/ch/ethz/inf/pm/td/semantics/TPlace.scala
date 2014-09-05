
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Place
 *
 * A named location
 *
 * @author Lucas Brutschy
 */ 

object TPlace extends AAny {

  /** Gets the category of the place */
  lazy val field_category = new TouchField("category",TString.typeName)

  /** Gets the link associated to the message */
  lazy val field_link = new TouchField("link",TString.typeName)

  /** Gets the location of the place */
  lazy val field_location = new TouchField("location",TLocation.typeName)

  /** Gets the name of the place */
  lazy val field_name = new TouchField("name",TString.typeName)

  /** Gets a url to the picture */
  lazy val field_picture_link = new TouchField("picture link",TString.typeName)

  /** Gets the source of this place (facebook, touchdevelop) */
  lazy val field_source = new TouchField("source",TString.typeName)

  lazy val typeName = TypeName("Place")

  override def possibleFields = super.possibleFields ++ List(field_category, field_link, field_location,
    field_name, field_picture_link, field_source)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Checks into the place (supported for Facebook) */
    case "check in" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
