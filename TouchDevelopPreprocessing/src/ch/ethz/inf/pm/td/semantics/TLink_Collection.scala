
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Link Collection
 *
 * A list of links
 *
 * @author Lucas Brutschy
 */ 

object TLink_Collection extends AMutable_Collection {

  lazy val typeName = TypeName("Link Collection")

  def keyTypeName = TNumber.typeName
  def valueTypeName = TLink.typeName

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
