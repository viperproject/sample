
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Enumerator
 *
 * An general enumerator
 *
 * @author Lucas Brutschy
 */ 

object TEnumerator extends AAny {

  lazy val typeName = TypeName("Enumerator")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Return current value */
    // case "current" => 
    //   val List() = parameters // 
    //   Skip

    /** Advance enumerator and return true if there is another element. */
    // case "move next" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TBoolean)
    // DECLARATION AS FIELD: 
    //   /** Advance enumerator and return true if there is another element. */
    //   lazy val field_move_next = new TouchField("move next",TBoolean.typeName)

    // FIELDS: field_move_next

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
