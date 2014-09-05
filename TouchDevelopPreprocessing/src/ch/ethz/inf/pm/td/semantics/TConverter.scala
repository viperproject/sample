
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Converter
 *
 * A generic atomic conversion function
 *
 * @author Lucas Brutschy
 */ 

object TConverter extends AAny {

  lazy val typeName = TypeName("Converter")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Run the inline action. */
    // case "run" => 
    //   val List(elt) = parameters // From
    //   TopWithInvalid[S](TTo)
    // DECLARATION AS FIELD: 
    //   /** Run the inline action. */
    //   lazy val field_run = new TouchField("run",TTo.typeName)

    // FIELDS: field_run

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
