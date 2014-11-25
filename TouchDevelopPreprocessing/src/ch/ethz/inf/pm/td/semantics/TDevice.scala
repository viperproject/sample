
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Device
 *
 * A device on the home network
 *
 * @author Lucas Brutschy
 */ 

object TDevice extends AAny {

  /** Checks if the device is connected */
  lazy val field_is_connected = new ApiField("is connected",TBoolean.typeName)

  /** Gets the manfacturer name */
  lazy val field_manufacturer = new ApiField("manufacturer",TString.typeName)

  /** Gets the friendly name of the device */
  lazy val field_name = new ApiField("name",TString.typeName)

  lazy val typeName = TypeName("Device")

  override def possibleFields = super.possibleFields ++ List(field_is_connected, field_manufacturer, field_name)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Browses to the device control panel */
    case "browse" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
