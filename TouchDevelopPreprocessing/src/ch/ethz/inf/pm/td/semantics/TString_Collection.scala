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

  val typName = "String Collection"
  val typ = TouchCollection(typName,TNumber.typName,TString.typName)

}

class TString_Collection extends AMutable_Collection {

  def getTyp = TString_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Concatenates the separator and items into a string */
    case "join" =>
      val List(separator) = parameters // String
      Top[S](TString.typ)

    /** Sorts the strings in this collection */
    case "sort" =>
      CollectionInvalidateKeys(this0)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
