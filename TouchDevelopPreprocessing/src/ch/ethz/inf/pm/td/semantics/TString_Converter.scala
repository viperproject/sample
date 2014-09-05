
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of String Converter
 *
 * An atomic conversion function to string
 *
 * @author Lucas Brutschy
 */ 

object TString_Converter extends AAny {

  val typeName = TypeName("String Converter")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Run the inline action. */
    case "run" =>
      TopWithInvalid[S](TString,"inline action may return invalid (not checked)")

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
