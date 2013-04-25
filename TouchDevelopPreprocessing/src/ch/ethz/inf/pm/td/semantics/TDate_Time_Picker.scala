
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Date Time Picker
 *
 * A picker for time or date
 *
 * @author Lucas Brutschy
 */ 

object TDate_Time_Picker {

  val typName = "Date_Time_Picker"
  val typ = new TouchType(typName,isSingleton = true)

}

class TDate_Time_Picker extends AAny {

  def getTyp = TDate_Time_Picker.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    // FIELDS: 

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
