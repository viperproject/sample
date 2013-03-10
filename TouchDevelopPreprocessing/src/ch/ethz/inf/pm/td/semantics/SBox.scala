
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of box
 *
 * Based on "guessing" the semantics - no documentation available
 *
 * @author Lucas Brutschy
 */ 

object SBox {

  val field_init = new TouchField("init",TBoolean.typ)

  val typName = "box"
  val typ = new TouchType(typName,isSingleton = true,List())

}

class SBox extends AAny {

  def getTyp = SBox.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    case "is_init" =>
      Return[S](Field[S](this0,SBox.field_init))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}




      
