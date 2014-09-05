
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Media Link Collection
 *
 * A list of media links on the home network
 *
 * @author Lucas Brutschy
 */ 

object TMedia_Link_Collection extends ALinearCollection {

  lazy val typeName = TypeName("Media Link Collection")

  def keyTypeName = TNumber.typeName

  def valueTypeName = TMedia_Link.typeName

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
