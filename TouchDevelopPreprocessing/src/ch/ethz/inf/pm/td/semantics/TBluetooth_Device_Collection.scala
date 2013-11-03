
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Device Collection
 *
 * A collection of bluetooth devices
 *
 * @author Lucas Brutschy
 */

object TBluetooth_Device_Collection {

  val typName = "Bluetooth Device Collection"
  val typ = TouchCollection(typName,TNumber.typName,TBluetooth_Device.typName, immutableCollection = true)

}

class TBluetooth_Device_Collection extends ALinearCollection {

  def getTyp = TBluetooth_Device_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
