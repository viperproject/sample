
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TGamepad
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Gamepad
 *
 * A snapshot of the gamepad state
 *
 * @author Lucas Brutschy
 */ 

object TGamepad extends Default_TGamepad {

    lazy val field_timestamp = ApiField("timestamp",TNumber)
    lazy val field_is_connected = ApiField("is connected",TBoolean)
    lazy val field_index = ApiField("index",TNumber)
    lazy val field_id = ApiField("id",TString)

    override lazy val possibleFields = super.possibleFields ++ Set(
      field_timestamp,
      field_is_connected,
      field_index,
      field_id
    )

}
      
