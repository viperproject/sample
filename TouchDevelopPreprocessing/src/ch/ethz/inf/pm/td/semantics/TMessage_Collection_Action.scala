
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Message Collection Action
 *
 * A place to hook up an action to execute in response to an event
 *
 * @author Lucas Brutschy
 */ 

object TMessage_Collection_Action {

  val typName = "Message Collection Action"
  val typ = DefaultTouchType(typName, fields = List(AAction.field_handlerName), isImmutable = true)

}

class TMessage_Collection_Action extends AAny {

  def getTyp = TMessage_Collection_Action.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
