
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of records
 *
 * Lists objects, tables and indexes defined in the current script
 *
 * @author Lucas Brutschy
 */

object SRecords extends ASingleton {

  lazy val typeName = TypeName("records")

  override def declarations = super.declarations ++ mkGetterSetters(TypeList.records.toList)
  override def possibleFields = super.possibleFields ++ TypeList.records

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
