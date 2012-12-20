package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.MethodSummaries
import ch.ethz.inf.pm.td.compiler.{TouchType, MethodSummaries, TouchCompiler}

/**
 * Specifies the abstract semantics of code
 *
 * Lists actions defined in the current script
 *
 * @author Lucas Brutschy
 */

object SCode {

  val typName = "code"
  val typ = TouchType(typName, isSingleton = true)

}

class SCode(compiler:TouchCompiler) extends AAny {

  def getTyp = SCode.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = {

    compiler.getCalledMethod(method,parameters map (_.getType())) match {
      case Some(methodDef) =>
        MethodSummaries.collect(pp,methodDef,state,parameters)
      case _ =>
        super.forwardSemantics(this0,method,parameters)
    }

  }

}