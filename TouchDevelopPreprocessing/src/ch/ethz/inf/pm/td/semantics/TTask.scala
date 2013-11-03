
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Task
 *
 * A task created with `async` keyword
 *
 * @author Lucas Brutschy
 */ 

object TTask {

  /** Check if the task is done yet */
  val field_completed = new TouchField("completed",TBoolean.typName)

  val typName = "Task"
  val typ = new TouchType(typName,isSingleton = false,fields = List(field_completed))

}

class TTask extends AAny {

  def getTyp = TTask.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
