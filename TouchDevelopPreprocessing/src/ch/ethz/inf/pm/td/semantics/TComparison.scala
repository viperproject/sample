
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Comparison
 *
 * An atomic comparison action
 *
 * @author Lucas Brutschy
 */ 

object TComparison {

  val typName = "Comparison"
  val typ = DefaultTouchType(typName)

}

class TComparison extends AAny {

  def getTyp = TComparison.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Run the inline action. */
    case "run" =>
    //   val List(a,b) = parameters // Elt,Elt
      Top[S](TNumber.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
