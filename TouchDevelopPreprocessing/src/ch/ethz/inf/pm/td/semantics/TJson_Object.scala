
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

  /** Gets the list of keys */
  val field_keys = new TouchField("keys",TString_Collection.typ)

  /** Gets a json kind (string, number, object, array, boolean) */
  val field_kind = new TouchField("kind",TString.typ)

  /** Converts to a boolean (type must be boolean) */
  val field_to_boolean = new TouchField("to_boolean",TBoolean.typ)

  /** Converts to a number (type must be number) */
  val field_to_number = new TouchField("to_number",TNumber.typ)

  /** Converts to a number (type must be string) */
  val field_to_string = new TouchField("to_string",TString.typ)

  /** Converts and parses to a date time (type must be string) */
  val field_to_time = new TouchField("to_time",TDateTime.typ)

  val typName = "Json_Object"
  val typ = TouchCollection(typName,TString.typName,TJson_Object.typName,List(field_keys, field_kind, field_to_boolean, field_to_number, field_to_string, field_to_time))

}

class TJson_Object extends ACollection {

  def getTyp = TJson_Object.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets a field value as a boolean */
    case "boolean" =>
      val List(key) = parameters // String
      Return[S](Field[S](CollectionAt[S](this0,key),TJson_Object.field_to_boolean))

    /** Indicates if the key exists */
    case "contains_key" =>
      val List(key) = parameters // String
      Top[S](TBoolean.typ)

    /** Gets a value by name */
    case "field" =>
      val List(key) = parameters // String
      Return[S](CollectionAt[S](this0,key))

    /** Gets a field value as a number */
    case "number" =>
      val List(key) = parameters // String
      Return[S](Field[S](CollectionAt[S](this0,key),TJson_Object.field_to_number))

    /** Gets a field value as a string */
    case "string" =>
      val List(key) = parameters // String
      Return[S](Field[S](CollectionAt[S](this0,key),TJson_Object.field_to_string))

    /** Gets the field value as a time */
    case "time" =>
       val List(key) = parameters // String
       Return[S](Field[S](CollectionAt[S](this0,key),TJson_Object.field_to_time))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
