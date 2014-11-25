
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Task
 *
 * A task created with `async` keyword
 *
 * @author Lucas Brutschy
 */ 

object TTask extends AAny {

  /** Check if the task is done yet */
  lazy val field_completed = new ApiField("completed",TBoolean.typeName)

  val typeName = TypeName("Task")

  override def possibleFields = super.possibleFields ++ List(field_completed)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
