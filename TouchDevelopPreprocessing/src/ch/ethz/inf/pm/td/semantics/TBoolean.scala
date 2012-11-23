package ch.ethz.inf.pm.td.semantics

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
  val typ = TouchType(typName,isSingleton = false)

}

class TBoolean extends Any {

  def getTypeName = TBoolean.typName

  def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String,parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    case "and" => Expr(this0 && parameters.head)
    case "or" => Expr(this0 && parameters.head)

    case _ =>
      Unimplemented[S](this0.getType().toString+"."+method)

  }

}