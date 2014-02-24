package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}

class AField(fieldType:TouchType,valueField:TouchField) extends AAny {

  def getTyp = fieldType

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

     case "get" =>
       Return[S](Field[S](this0,valueField))

     case "set" =>
       val List(value) = parameters
       AssignField[S](this0,valueField,value)

     case "clear" =>
       AssignField[S](this0,valueField,Invalid(valueField.typ))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
