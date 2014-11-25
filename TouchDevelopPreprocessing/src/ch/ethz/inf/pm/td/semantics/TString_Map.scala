
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of String Map
 *
 * A map from strings to strings
 *
 * @author Lucas Brutschy
 */

object TString_Map extends AMap {

  /** Gets the keys in the map */
  lazy val field_keys = new ApiField("keys",TString_Collection.typeName)

  val typeName = TypeName("String Map")

  def keyTypeName = TString.typeName

  def valueTypeName = TString.typeName

  override def possibleFields = super.possibleFields ++ Set(field_keys)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the value at a given key; invalid if not found */
    // case "at" =>
    //   val List(key) = parameters // String
    //   Top[S](TString)

    /** Gets the number of elements in the map */
    // case "count" =>
    //   Top[S](TNumber)
    // DECLARATION AS FIELD:
    //   /** Gets the number of elements in the map */
    //   lazy val field_count = new TouchField("count",TNumber.typeName)

    /** Gets the keys in the map */
    //case "keys" =>
    //  Top[S](TString_Collection)
    // DECLARATION AS FIELD:

    /** Removes the value at a given key */
    // case "remove" =>
    //   val List(key) = parameters // String
    //   Skip;

    /** Sets the value at a given key; invalid if not found */
    // case "set at" =>
    //   val List(key,value) = parameters // String,String
    //   Skip;

    /** Sets many elements at once. */
    // case "set many" =>
    //   val List(other) = parameters // String_Map
    //   Skip;

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}