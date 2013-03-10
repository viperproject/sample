
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of records
 *
 * Lists objects, tables and indexes defined in the current script
 *
 * @author Lucas Brutschy
 */ 

object SRecords {

  val typName = "records"
  var typ = new TouchType(typName,isSingleton = true,List())

}

class SRecords extends AAny {

  def getTyp = SRecords.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
