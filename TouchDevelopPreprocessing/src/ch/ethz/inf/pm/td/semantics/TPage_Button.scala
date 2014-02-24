package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
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
  val field_text = new TouchField("text",TString.typName)

  /** Gets the page hosting this button */
  val field_page = new TouchField("page",TPage.typName)

  /** Gets the icon name */
  val field_icon = new TouchField("icon",TString.typName)

  val typName = "Page Button"
  val typ = DefaultTouchType(typName,isSingleton = false, fields = List(field_text,field_page,field_icon))

}

class TPage_Button extends AAny {

  def getTyp = TPage_Button.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets a value indicating if both instances are equal */
    // case "equals" => 
    //   val List(page_button) = parameters // Page_Button
    //   Top[S](TBoolean.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}