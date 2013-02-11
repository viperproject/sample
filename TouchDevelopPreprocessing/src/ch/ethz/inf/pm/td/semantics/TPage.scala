package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Page
 *
 * A page on a wall
 *
 * @author Lucas Brutschy
 */

object TPage {

  val typName = "Page"
  val typ = new TouchType(typName,isSingleton = false,List())

}

class TPage extends AAny {

  def getTyp = TPage.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets a value indicating if the page is equal to the other */
    // case "equals" => 
    //   val List(other) = parameters // Page
    //   Return[S](Valid(TBoolean.typ))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}