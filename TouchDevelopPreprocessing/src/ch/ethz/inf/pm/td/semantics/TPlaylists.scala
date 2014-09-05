
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Playlists
 *
 * A collection of playlists
 *
 * @author Lucas Brutschy
 */ 

object TPlaylists extends ALinearCollection {

  lazy val typeName = TypeName("Playlists")

  def keyTypeName = TNumber.typeName

  def valueTypeName = TPlaylist.typeName

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
