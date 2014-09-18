package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TString_Collection extends AMutable_Collection {

  val typeName = TypeName("String Collection")

  def keyTypeName = TNumber.typeName
  def valueTypeName = TString.typeName

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Concatenates the separator and items into a string */
    case "join" =>
      val List(separator) = parameters // String
      Top[S](TString)

    /** Sorts the strings in this collection */
    case "sort" =>
      collectionInvalidateKeys(this0)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
