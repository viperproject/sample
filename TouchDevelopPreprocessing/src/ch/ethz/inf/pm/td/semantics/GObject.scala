package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics, MethodSummaries}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

case class GObject(typeName:TypeName, fields:List[ApiField]) extends AAny {

  override def possibleFields = super.possibleFields ++ fields

  override lazy val declarations = super.declarations ++ mkGetterSetters(fields) ++ Map("equals" -> member_equals)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case "clear fields" =>
      //      var curState = state
      //      for (field <- objectTyp.possibleTouchFields) {
      //        curState = CallApi[S](Field[S](this0,field),"clear",Nil,TUnknown)(curState,pp)
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
