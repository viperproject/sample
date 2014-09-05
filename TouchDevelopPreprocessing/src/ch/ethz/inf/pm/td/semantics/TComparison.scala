
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Comparison
 *
 * An atomic comparison action
 *
 * @author Lucas Brutschy
 */ 

object TComparison extends AAny {

  lazy val typeName = TypeName("Comparison")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Run the inline action. */
    case "run" =>
    //   val List(a,b) = parameters // Elt,Elt
      Top[S](TNumber)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
