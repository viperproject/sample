package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{CFGGenerator, TouchType}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._

/**
 * This is empty but needs to be there as a type
 *
 * @author Lucas Brutschy
 */

object SData extends ASingleton {

  lazy val typeName = TypeName("data",isSingleton = true)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = {

    Return[S](Data(method,returnedType))

  }
}
