
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Picture Albums
 *
 * A collection of picture albums
 *
 * @author Lucas Brutschy
 */ 

object TPicture_Albums {

  val typName = "Picture Albums"
  val typ = TouchCollection(typName,"Number","Picture Album", immutableCollection = true)

}

class TPicture_Albums extends ACollection {

  def getTyp = TPicture_Albums.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
