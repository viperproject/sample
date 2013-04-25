package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TBoolean {

  val typName = "Boolean"
  val typ = new TouchType(typName,isSingleton = false)

}

class TBoolean extends AAny {

  def getTyp = TBoolean.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String,parameters:List[ExpressionSet],returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    case "and" => Return(this0 && parameters.head)
    case "or" => Return(this0 || parameters.head)
    case "not" => Return(this0.not())
    case "equals" => Return[S]((this0 && parameters.head)||(this0.not() && parameters.head.not()))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}