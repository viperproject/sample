package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{Constant, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * This is empty but needs to be there as a type
 *
 * @author Lucas Brutschy
 */

object SLibs extends ASingleton {

  lazy val typeName = TypeName("♻")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = {

    lazy val typ = new TouchType {
      override def typeName: TypeName = TypeName("♻"+method)
      override def isSingleton: Boolean = true
      override def isImmutable: Boolean = true
    }

    state.setExpression(ExpressionSet(Constant("", typ, pp)))

  }
}
