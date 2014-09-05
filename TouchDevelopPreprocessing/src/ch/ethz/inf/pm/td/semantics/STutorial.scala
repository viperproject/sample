
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Tutorial
 *
 * Support for interactive tutorials.
 *
 * @author Lucas Brutschy
 */ 

object STutorial extends ASingleton {

  lazy val typeName = TypeName("Tutorial")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** [**dbg**] Show a suggestion to the user (eg., an error description) */
    case "show hint" =>
    //   val List(message) = parameters // String
      Skip

    /** Signal that the step is done. */
    case "step completed" =>
    //   val List() = parameters // 
      Skip

    // FIELDS: 

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
