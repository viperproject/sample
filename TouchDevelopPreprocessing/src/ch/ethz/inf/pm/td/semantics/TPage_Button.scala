package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Page Button
 *
 * A page button on the wall
 *
 * @author Lucas Brutschy
 */

object TPage_Button {

  /** Gets the text */
  val field_text = new TouchField("text",TString.typ)

  /** Gets the page hosting this button */
  val field_page = new TouchField("page",TPage.typ)

  /** Gets the icon name */
  val field_icon = new TouchField("icon",TString.typ)

  val typName = "Page_Button"
  val typ = TouchType(typName,isSingleton = false,List(field_text,field_page,field_icon))

}

class TPage_Button extends AAny {

  def getTyp = TPage_Button.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets a value indicating if both instances are equal */
    // case "equals" => 
    //   val List(page_button) = parameters // Page_Button
    //   Return[S](Valid(TBoolean.typ))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}