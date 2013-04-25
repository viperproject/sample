
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Sound Recorder
 *
 * A sound recorder
 *
 * @author Lucas Brutschy
 */ 

object TSound_Recorder {

  val typName = "Sound Recorder"
  val typ = new TouchType(typName,isSingleton = true)

}

class TSound_Recorder extends AAny {

  def getTyp = TSound_Recorder.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    // FIELDS: 

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
