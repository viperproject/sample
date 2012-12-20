package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichExpression._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TBoolean {

  val typName = "Boolean"
  val typ = TouchType(typName,isSingleton = false)

}

class TBoolean extends AAny {

  def getTyp = TBoolean.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String,parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    case "and" => Return(this0 && parameters.head)
    case "or" => Return(this0 && parameters.head)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}