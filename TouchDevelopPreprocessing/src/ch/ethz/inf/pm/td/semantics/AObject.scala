package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import RichNativeSemantics._

class AObject(objectTyp:TouchType) extends AAny {

  def getTyp = objectTyp

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case "clear_fields" =>
      var curState = state
      for (field <- objectTyp.getPossibleTouchFields()) {
        curState = AssignField[S](this0,field,Invalid(field.getType()))(curState,pp)
      }
      curState

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
