
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._
import ch.ethz.inf.pm.td.semantics.TNumber_Collection._

/**
 * Specifies the abstract semantics of Json Object
 *
 * A json data structure
 *
 * @author Lucas Brutschy
 */

object TJson_Object extends AMap {

  /** Gets the list of keys */
  lazy val field_keys = new TouchField("keys", TString_Collection.typeName)

  /** Gets a json kind (string, number, object, array, boolean, null) */
  lazy val field_kind = new TouchField("kind", TString.typeName)

  /** Converts to a boolean (type must be boolean) */
  lazy val field_to_boolean = new TouchField("to boolean", TBoolean.typeName)

  /** Converts to a number (type must be number) */
  lazy val field_to_number = new TouchField("to number", TNumber.typeName)

  /** Converts to a number (type must be string) */
  lazy val field_to_string = new TouchField("to string", TString.typeName)

  /** Converts and parses to a date time (type must be string) */
  lazy val field_to_time = new TouchField("to time", TDateTime.typeName)

  lazy val typeName = TypeName("Json Object")

  def keyTypeName = TString.typeName
  def valueTypeName = TJson_Object.typeName

  override def possibleFields = super.possibleFields ++ Set(field_keys, field_kind, field_to_boolean, field_to_number,
    field_to_string, field_to_time)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Gets the i-th element */
    case "at index" =>
      val List(index) = parameters
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive(index, 0, CollectionSize[S](this0) - NumericalAnalysisConstants.epsilon, "at index", "index")
      Return[S](collectionAllValues[S](this0))

    /** Gets a field value as a boolean */
    case "boolean" =>
      val List(key) = parameters // String
      Return[S](Field[S](collectionAt[S](this0, key), TJson_Object.field_to_boolean))

    /** Indicates if the key exists */
    case "contains key" =>
      val List(key) = parameters // String
      Return[S](collectionContainsKey[S](this0, key))

    /** Gets a value by name */
    case "field" =>
      super.forwardSemantics(this0, "at", parameters, returnedType)

    /** Create a string formatted for easy readability */
    case "format" =>
      val List(spaces) = parameters // Number
      Top[S](TString)

    /** Gets a field value as a number */
    case "number" =>
      val List(key) = parameters // String
      Return[S](Field[S](collectionAt[S](this0, key), TJson_Object.field_to_number))

    /** Gets a field value as a string */
    case "string" =>
      Return[S](Field[S](super.forwardSemantics(this0, "at", parameters, returnedType).expr, TJson_Object.field_to_string))

    /** Gets the field value as a time */
    case "time" =>
      val List(key) = parameters // String
      Return[S](Field[S](collectionAt[S](this0, key), TJson_Object.field_to_time))

    /** Copy current JSON object into a Json Builder so it can be modified */
    case "to json builder" =>
      Top[S](TJson_Builder, initials = Map(
        TJson_Builder.field_keys -> Field[S](this0, TJson_Object.field_keys),
        TJson_Builder.field_kind -> Field[S](this0, TJson_Object.field_kind),
        TJson_Builder.field_to_boolean -> Field[S](this0, TJson_Object.field_to_boolean),
        TJson_Builder.field_to_number -> Field[S](this0, TJson_Object.field_to_number),
        TJson_Builder.field_to_string -> Field[S](this0, TJson_Object.field_to_string),
        TJson_Builder.field_to_time -> Field[S](this0, TJson_Object.field_to_time)
      ))

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
