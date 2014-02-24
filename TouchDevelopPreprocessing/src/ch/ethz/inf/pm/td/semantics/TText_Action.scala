
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Text Action
 *
 * A place to hook up an action to execute in response to an event
 *
 * @author Lucas Brutschy
 */ 

object TText_Action {

  val typName = "Text Action"
  val typ = DefaultTouchType(typName, fields = List(AAction.field_handlerName), isImmutable = true)

}

class TText_Action extends AAny {

  def getTyp = TText_Action.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
