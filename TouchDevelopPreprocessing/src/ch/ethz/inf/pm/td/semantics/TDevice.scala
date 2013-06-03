
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Device
 *
 * A device on the home network
 *
 * @author Lucas Brutschy
 */ 

object TDevice {

  /** Checks if the device is connected */
  val field_is_connected = new TouchField("is connected",TBoolean.typName)

  /** Gets the manfacturer name */
  val field_manufacturer = new TouchField("manufacturer",TString.typName)

  /** Gets the friendly name of the device */
  val field_name = new TouchField("name",TString.typName)

  val typName = "Device"
  val typ = new TouchType(typName,isSingleton = false, fields = List(field_is_connected, field_manufacturer, field_name))

}

class TDevice extends AAny {

  def getTyp = TDevice.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Browses to the device control panel */
    case "browse" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
