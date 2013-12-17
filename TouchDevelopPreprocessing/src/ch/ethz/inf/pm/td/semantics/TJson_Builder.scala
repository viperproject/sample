
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Json Builder
 *
 * A json data structure builder
 *
 * @author Lucas Brutschy
 */ 

object TJson_Builder {

  /** Gets the list of keys */
  val field_keys = new TouchField("keys",TString_Collection.typName)

  /** Gets a json kind (string, number, object, array, boolean, null) */
  val field_kind = new TouchField("kind",TString.typName)

  /** Converts to a boolean (type must be boolean) */
  val field_to_boolean = new TouchField("to boolean",TBoolean.typName)

  /** Converts to a number (type must be number) */
  val field_to_number = new TouchField("to number",TNumber.typName)

  /** Converts to a number (type must be string) */
  val field_to_string = new TouchField("to string",TString.typName)

  /** Converts and parses to a date time (type must be string) */
  val field_to_time = new TouchField("to time",TDateTime.typName)

  val typName = "Json Builder"
  val typ = TouchCollection(typName,TString.typName,TJson_Builder.typName,List(field_keys, field_kind, field_to_boolean, field_to_number, field_to_string, field_to_time), immutableCollection = true)


}

class TJson_Builder extends AMap {

  def getTyp = TJson_Builder.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {


    /** Add a reference to JsonBuilder to the array. */
    // case "add builder" =>
    //   val List(value) = parameters // Json_Builder
    //   Skip

    /** Adds a null value to the array. */
    // case "add null" =>
    //   val List() = parameters //
    //   Skip

    /** Adds a value to the array. */
    // case "add" =>
    //   val List(value) = parameters // Json_Object
    //   Skip

    /** Stringify the current JSON object */
    case "serialize" =>
       val List() = parameters //
       Top[S](TString.typ)

    /** Sets the field value. */
    case "set field" =>
      val List(name,value) = parameters // String,Json_Object
      CallApi[S](this0,"set at",List(name,value),TNothing.typ)

    /** Sets the string value. */
    case "set string" =>
      val List(name,value) = parameters // String,String
      val curState = New[S](TJson_Builder.typ,Map(
        TJson_Builder.field_kind -> String("string"),
        TJson_Builder.field_to_string -> value
      ))
      CallApi[S](this0,"set at",List(name,curState.getExpression),TNothing.typ)(curState,pp)

    /** Sets the field the the reference to JsonBuilder. */
    case "set builder" =>
      val List(name,value) = parameters // String,Json_Builder
      val curState = Top[S](TJson_Builder.typ)
      CallApi[S](this0,"set at",List(name,curState.getExpression),TNothing.typ)(curState,pp)

    /** Sets the Picture value as a data uri. */
    case "set picture" =>
      val List(name,pic,quality) = parameters // String,Picture,Number
      val curState = Top[S](TJson_Builder.typ)
      CallApi[S](this0,"set at",List(name,curState.getExpression),TNothing.typ)(curState,pp)

    /** Sets the Sound value as a data uri. */
    case "set sound" =>
      val List(name,snd) = parameters // String,Sound
      val curState = Top[S](TJson_Builder.typ)
      CallApi[S](this0,"set at",List(name,curState.getExpression),TNothing.typ)(curState,pp)

    /** Sets the number value. */
    case "set number" =>
      val List(name,value) = parameters // String,Number
      val curState = New[S](TJson_Builder.typ,Map(
        TJson_Builder.field_kind -> String("number"),
        TJson_Builder.field_to_number -> value
      ))
      CallApi[S](this0,"set at",List(name,curState.getExpression),TNothing.typ)(curState,pp)

    /** Sets the boolean value. */
    case "set boolean" =>
      val List(name,value) = parameters // String,Boolean
      val curState = New[S](TJson_Builder.typ,Map(
        TJson_Builder.field_kind -> String("boolean"),
        TJson_Builder.field_to_boolean -> value
      ))
      CallApi[S](this0,"set at",List(name,curState.getExpression),TNothing.typ)(curState,pp)

    /** Sets the field value as null. */
    case "set field null" =>
      val List(name) = parameters // String
      CallApi[S](this0,"remove at",List(name),TNothing.typ)

    /** Deletes named field */
    case "remove field" =>
       val List(name) = parameters // String
       CollectionRemove[S](this0,name)

    // ---- ALL THE STANDARD STUFF

    /** Gets the i-th element */
    case "at index" =>
      val List(index) = parameters
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive(index, 0, CollectionSize[S](this0) - NumericalAnalysisConstants.epsilon, "at index", "index")
      Return[S](CollectionSummary[S](this0))

    /** Gets a field value as a boolean */
    case "boolean" =>
      val List(key) = parameters // String
      Return[S](Field[S](CollectionAt[S](this0,key),TJson_Builder.field_to_boolean))

    /** Indicates if the key exists */
    case "contains key" =>
      val List(key) = parameters // String
      Return[S](CollectionContainsKey[S](this0, key))

    /** Gets a value by name */
    case "field" =>
      super.forwardSemantics(this0,"at",parameters,returnedType)

    /** Gets a field value as a number */
    case "number" =>
      val List(key) = parameters // String
      Return[S](Field[S](CollectionAt[S](this0,key),TJson_Builder.field_to_number))

    /** Gets a field value as a string */
    case "string" =>
      Return[S](Field[S](super.forwardSemantics(this0,"at",parameters,returnedType).getExpression, TJson_Builder.field_to_string))

    /** Gets the field value as a time */
    case "time" =>
      val List(key) = parameters // String
      Return[S](Field[S](CollectionAt[S](this0,key),TJson_Builder.field_to_time))

    /** Copy current JSON object into a Json Builder so it can be modified */
    case "to json" =>
      Top[S](TJson_Object.typ,initials = Map (
        TJson_Object.field_keys -> Field[S](this0,TJson_Builder.field_keys),
        TJson_Object.field_kind -> Field[S](this0,TJson_Builder.field_kind),
        TJson_Object.field_to_boolean -> Field[S](this0,TJson_Builder.field_to_boolean),
        TJson_Object.field_to_number -> Field[S](this0,TJson_Builder.field_to_number),
        TJson_Object.field_to_string -> Field[S](this0,TJson_Builder.field_to_string),
        TJson_Object.field_to_time -> Field[S](this0,TJson_Builder.field_to_time)
      ))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
