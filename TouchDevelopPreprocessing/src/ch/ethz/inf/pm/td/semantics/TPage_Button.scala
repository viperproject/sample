package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.TouchField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Page Button
 *
 * A page button on the wall
 *
 * @author Lucas Brutschy
 */

object TPage_Button extends AAny {

  /** Gets the text */
  lazy val field_text = new TouchField("text",TString.typeName)

  /** Gets the page hosting this button */
  lazy val field_page = new TouchField("page",TPage.typeName)

  /** Gets the icon name */
  lazy val field_icon = new TouchField("icon",TString.typeName)

  lazy val typeName = TypeName("Page Button")

  override def possibleFields = super.possibleFields ++ List(field_text,field_page,field_icon)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets a value indicating if both instances are equal */
    // case "equals" => 
    //   val List(page_button) = parameters // Page_Button
    //   Top[S](TBoolean)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}