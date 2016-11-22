/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TDevice
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Device
 *
 * A device on the home network
 *
 * @author Lucas Brutschy
 */ 

object TDevice extends Default_TDevice {

  /** Checks if the device is connected */
  lazy val field_is_connected = ApiField("is connected", TBoolean)

  /** Gets the manfacturer name */
  lazy val field_manufacturer = ApiField("manufacturer", TString)

  /** Gets the friendly name of the device */
  lazy val field_name = ApiField("name", TString)

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
