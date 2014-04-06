
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.interpreter.{ConcreteInterpreter, TouchValue}

/**
 * Specifies the abstract semantics of Json Object
 *
 * A json data structure
 *
 * @author Lucas Brutschy
 */

object TJson_Object {

  /** Gets the list of keys */
  val field_keys = new TouchField("keys", TString_Collection.typName)

  /** Gets a json kind (string, number, object, array, boolean, null) */
  val field_kind = new TouchField("kind", TString.typName)

  /** Converts to a boolean (type must be boolean) */
  val field_to_boolean = new TouchField("to boolean", TBoolean.typName)

  /** Converts to a number (type must be number) */
  val field_to_number = new TouchField("to number", TNumber.typName)

  /** Converts to a number (type must be string) */
  val field_to_string = new TouchField("to string", TString.typName)

  /** Converts and parses to a date time (type must be string) */
  val field_to_time = new TouchField("to time", TDateTime.typName)

  val typName = "Json Object"
  val typ = TouchCollection(typName, TString.typName, TJson_Object.typName, List(field_keys, field_kind, field_to_boolean, field_to_number, field_to_string, field_to_time), immutableCollection = true)

}

class TJson_Object extends AMap {

  def getTyp = TJson_Object.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Gets the i-th element */
    case "at index" =>
      val List(index) = parameters
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive(index, 0, CollectionSize[S](this0) - NumericalAnalysisConstants.epsilon, "at index", "index")
      Return[S](CollectionSummary[S](this0))

    /** Gets a field value as a boolean */
    case "boolean" =>
      val List(key) = parameters // String
      Return[S](Field[S](CollectionAt[S](this0, key), TJson_Object.field_to_boolean))

    /** Indicates if the key exists */
    case "contains key" =>
      val List(key) = parameters // String
      Return[S](CollectionContainsKey[S](this0, key))

    /** Gets a value by name */
    case "field" =>
      super.forwardSemantics(this0, "at", parameters, returnedType)

    /** Create a string formatted for easy readability */
    case "format" =>
      val List(spaces) = parameters // Number
      Top[S](TString.typ)

    /** Gets a field value as a number */
    case "number" =>
      val List(key) = parameters // String
      Return[S](Field[S](CollectionAt[S](this0, key), TJson_Object.field_to_number))

    /** Gets a field value as a string */
    case "string" =>
      Return[S](Field[S](super.forwardSemantics(this0, "at", parameters, returnedType).expr, TJson_Object.field_to_string))

    /** Gets the field value as a time */
    case "time" =>
      val List(key) = parameters // String
      Return[S](Field[S](CollectionAt[S](this0, key), TJson_Object.field_to_time))

    /** Copy current JSON object into a Json Builder so it can be modified */
    case "to json builder" =>
      Top[S](TJson_Builder.typ, initials = Map(
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

  override def concreteSemantics(this0: TouchValue,
                                 method: String,
                                 params: List[TouchValue],
                                 interpreter: ConcreteInterpreter,
                                 pp: ProgramPoint): TouchValue = method match {
    case "field" =>
      super.concreteSemantics(this0, "at", params, interpreter, pp)

    case _ =>
      super.concreteSemantics(this0, method, params, interpreter, pp)
  }
}
      
