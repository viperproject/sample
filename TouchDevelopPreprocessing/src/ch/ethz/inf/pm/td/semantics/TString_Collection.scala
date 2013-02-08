package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TString_Collection {

  val typName = "String_Collection"
  val typ = TouchCollection(typName,"Number","String")

}

class TString_Collection extends AMutable_Collection {

  def getTyp = TString_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Indicates if the collection contains the item */
    case "contains" =>
      val List(item) = parameters // String
      Return[S](True or False)

    /** Concatenates the separator and items into a string */
    case "join" =>
      val List(separator) = parameters // String
      Return[S](Valid(TString.typ))

    /** Sorts the strings in this collection */
    case "sort" =>
      Skip; // Sort is invariant for our collection representation

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
