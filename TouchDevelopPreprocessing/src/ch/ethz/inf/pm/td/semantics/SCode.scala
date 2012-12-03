package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{MethodSummaries, TouchCompiler}

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */
class SCode(compiler:TouchCompiler) extends Any {

  def getTypeName = "code"

  def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = {

    // TODO: ARGUMENTS AND RETURN VALUES
    compiler.getCalledMethod(method,parameters map (_.getType())) match {
      case Some(methodDef) =>
        MethodSummaries.collect(pp,methodDef,state)
      case None =>
        Unimplemented[S](this0.getType().toString+"."+method)
    }

  }

}