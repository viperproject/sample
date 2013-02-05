
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Printer Collection
 *
 * A collection of printers
 *
 * @author Lucas Brutschy
 */ 

object TPrinter_Collection {

  val typName = "Printer_Collection"
  val typ = TouchCollection(typName,TNumber.typ,TPrinter.typ)

}

class TPrinter_Collection extends ACollection {

  def getTyp = TPrinter_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
