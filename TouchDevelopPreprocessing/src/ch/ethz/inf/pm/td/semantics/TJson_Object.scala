
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Json Object
 *
 * A json data structure
 *
 * @author Lucas Brutschy
 */ 

object TJson_Object {

  val typName = "Json_Object"
  val typ = TouchCollection(typName,"Number","Json_Object")

}

class TJson_Object extends ACollection {

  def getTyp = TJson_Object.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets a field value as a boolean */
    // case "boolean" => 
    //   val List(key) = parameters // String
    //   Top[S](TBoolean.typ)

    /** Indicates if the key exists */
    // case "contains_key" => 
    //   val List(key) = parameters // String
    //   Top[S](TBoolean.typ)

    /** Gets a value by name */
    case "field" =>
      val List(key) = parameters // String
      Return[S](CollectionSummary[S](this0))

    /** Gets the list of keys */
    // case "keys" => 
    //   Top[S](TString_Collection.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the list of keys */
    //   val field_keys = new TouchField("keys",TString_Collection.typ)

    /** Gets a json kind (string, number, object, array, boolean) */
    // case "kind" =>
    //   Top[S](TString.typ)
    // DECLARATION AS FIELD:
    //   /** Gets a json kind (string, number, object, array, boolean) */
    //   val field_kind = new TouchField("kind",TString.typ)

    /** Gets a field value as a number */
    // case "number" => 
    //   val List(key) = parameters // String
    //   Top[S](TNumber.typ)

    /** Gets a field value as a string */
    // case "string" => 
    //   val List(key) = parameters // String
    //   Top[S](TString.typ)

    /** Gets the field value as a time */
    // case "time" => 
    //   val List(key) = parameters // String
    //   Top[S](TDateTime.typ)

    /** Converts to a boolean (type must be boolean) */
    // case "to_boolean" => 
    //   Top[S](TBoolean.typ)
    // DECLARATION AS FIELD: 
    //   /** Converts to a boolean (type must be boolean) */
    //   val field_to_boolean = new TouchField("to_boolean",TBoolean.typ)

    /** Converts to a number (type must be number) */
    // case "to_number" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Converts to a number (type must be number) */
    //   val field_to_number = new TouchField("to_number",TNumber.typ)

    /** Converts and parses to a date time (type must be string) */
    // case "to_time" => 
    //   Top[S](TDateTime.typ)
    // DECLARATION AS FIELD: 
    //   /** Converts and parses to a date time (type must be string) */
    //   val field_to_time = new TouchField("to_time",TDateTime.typ)

    // FIELDS: , field_count, field_keys, field_kind, field_to_boolean, field_to_number, field_to_string, field_to_time

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
