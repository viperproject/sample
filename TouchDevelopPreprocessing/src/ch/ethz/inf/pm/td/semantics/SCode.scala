package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of code
 *
 * Lists actions defined in the current script
 *
 * @author Lucas Brutschy
 */

object SCode {

  val typName = "code"
  val typ = new TouchType(typName, isSingleton = true)

}

class SCode extends AAny {

  def getTyp = SCode.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = {

    CallLocalAction[S](method,parameters)

  }

}