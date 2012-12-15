package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{TouchType, MethodSummaries, TouchCompiler}

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SCode {

  val typName = "code"
  val typ = TouchType(typName, isSingleton = true)

}

class SCode(compiler:TouchCompiler) extends AAny {

  def getTyp = SCode.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = {

    // TODO: ARGUMENTS AND RETURN VALUES
    compiler.getCalledMethod(method,parameters map (_.getType())) match {
      case Some(methodDef) =>
        MethodSummaries.collect(pp,methodDef,state)
      case _ =>
        super.forwardSemantics(this0,method,parameters)
    }

  }

}