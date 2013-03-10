package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import scala.Predef.String

class ANumberField(fieldType:TouchType,valueField:TouchField) extends AField(fieldType,valueField) {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case "add" =>
      val List(other) = parameters
      AssignField[S](this0,valueField,Field[S](this0,valueField) + other)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
