package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Contact Collection
 *
 * A collection of contacts
 *
 * @author Lucas Brutschy
 */

object TContact_Collection {

  val typName = "Contact Collection"
  val typ = TouchCollection(typName,"Number","Contact", immutableCollection = true)

}

class TContact_Collection extends ACollection {

  def getTyp = TContact_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}