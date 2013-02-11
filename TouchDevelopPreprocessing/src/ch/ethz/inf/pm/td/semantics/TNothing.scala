package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType


/**
 * Specifies the abstract semantics of Nothing
 *
 * Nothing
 *
 * @author Lucas Brutschy
 */
object TNothing {

  val typName = "Nothing"
  val typ = new TouchType(typName,isSingleton = false)

}

class TNothing extends AAny {

  def getTyp = TNothing.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String,parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
