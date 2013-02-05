
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Device Collection
 *
 * A collection of devices
 *
 * @author Lucas Brutschy
 */ 

object TDevice_Collection {

  val typName = "Device_Collection"
  val typ = TouchCollection(typName,TNumber.typ,TDevice.typ)

}

class TDevice_Collection extends ACollection {

  def getTyp = TDevice_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
