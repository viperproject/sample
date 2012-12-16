package ch.ethz.inf.pm.td.semantics

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

  val typName = "Json Object"
  val typ:TouchCollection = TouchCollection(typName,TNumber.typ,TJson_Object.typ)

}

class TJson_Object extends ACollection {

  def getTyp = TJson_Object.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets a field value as a boolean */
    // case "boolean" => 
    //   val List(key) = parameters // String
    //   Return[S](Valid(TBoolean.typ))

    /** Indicates if the key exists */
    // case "contains_key" => 
    //   val List(key) = parameters // String
    //   Return[S](Valid(TBoolean.typ))

    /** Gets a value by name */
    // case "field" => 
    //   val List(key) = parameters // String
    //   Return[S](Valid(TJson_Object.typ))

    /** Gets the list of keys */
    // case "keys" => 
    //   Return[S](Valid(TString_Collection.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the list of keys */
    //   field_keys = new TouchField("keys",TString_Collection.typ)

    /** Gets a json kind (string, number, object, array, boolean) */
    // case "kind" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets a json kind (string, number, object, array, boolean) */
    //   field_kind = new TouchField("kind",TString.typ)

    /** Gets a field value as a number */
    // case "number" => 
    //   val List(key) = parameters // String
    //   Return[S](Valid(TNumber.typ))

    /** Gets a field value as a string */
    // case "string" => 
    //   val List(key) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Gets the field value as a time */
    // case "time" => 
    //   val List(key) = parameters // String
    //   Return[S](Valid(TDateTime.typ))

    /** Converts to a boolean (type must be boolean) */
    // case "to_boolean" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Converts to a boolean (type must be boolean) */
    //   field_to_boolean = new TouchField("to_boolean",TBoolean.typ)

    /** Converts to a number (type must be number) */
    // case "to_number" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Converts to a number (type must be number) */
    //   field_to_number = new TouchField("to_number",TNumber.typ)

    /** Converts to a string (type must be string) */
    // case "to_string" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Converts to a string (type must be string) */
    //   field_to_string = new TouchField("to_string",TString.typ)

    /** Converts and parses to a date time (type must be string) */
    // case "to_time" => 
    //   Return[S](Valid(TDateTime.typ))
    // DECLARATION AS FIELD: 
    //   /** Converts and parses to a date time (type must be string) */
    //   field_to_time = new TouchField("to_time",TDateTime.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}