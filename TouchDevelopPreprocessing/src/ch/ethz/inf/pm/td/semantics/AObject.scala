package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, TouchType}
import RichNativeSemantics._
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.analysis.MethodSummaries

class AObject(objectTyp:TouchType) extends AAny {

  def getTyp = objectTyp

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case "clear fields" =>
      //      var curState = state
      //      for (field <- objectTyp.possibleTouchFields) {
      //        curState = CallApi[S](Field[S](this0,field),"clear",Nil,TUnknown.typ)(curState,pp)
      //      }
      //      curState
      Skip[S]

    case _ =>

      // member method may be declared in libraries as code->(this,param1...)
      SystemParameters.compiler.asInstanceOf[TouchCompiler].getMethod(method, (this0 :: parameters).map {
        _.getType()
      }) match {
        case Some(x) => MethodSummaries.collect(pp, x, state, this0 :: parameters)
        case None => super.forwardSemantics(this0, method, parameters, returnedType)
      }

  }

}
