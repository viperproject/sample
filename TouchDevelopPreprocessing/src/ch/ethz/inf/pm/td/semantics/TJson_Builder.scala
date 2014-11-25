
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._
import ch.ethz.inf.pm.td.semantics.TNumber_Collection._

/**
 * Specifies the abstract semantics of Json Builder
 *
 * A json data structure builder
 *
 * @author Lucas Brutschy
 */ 

object TJson_Builder extends AMap {

  /** Gets the list of keys */
  lazy val field_keys = new ApiField("keys",TString_Collection.typeName)

  /** Gets a json kind (string, number, object, array, boolean, null) */
  lazy val field_kind = new ApiField("kind",TString.typeName)

  /** Converts to a boolean (type must be boolean) */
  lazy val field_to_boolean = new ApiField("to boolean",TBoolean.typeName)

  /** Converts to a number (type must be number) */
  lazy val field_to_number = new ApiField("to number",TNumber.typeName)

  /** Converts to a number (type must be string) */
  lazy val field_to_string = new ApiField("to string",TString.typeName)

  /** Converts and parses to a date time (type must be string) */
  lazy val field_to_time = new ApiField("to time",TDateTime.typeName)

  lazy val typeName = TypeName("Json Builder")

  def keyTypeName = TString.typeName

  def valueTypeName = TJson_Builder.typeName

  override def possibleFields = super.possibleFields ++ Set(field_keys, field_kind, field_to_boolean, field_to_number,
    field_to_string, field_to_time)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {


    /** Add a reference to JsonBuilder to the array. */
    //    case "add builder" =>
    //       this.forwardSemantics[S](this0,"add",parameters)

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
       Top[S](TString)

    /** Sets the field value. */
    case "set field" =>
      val List(name,value) = parameters // String,Json_Object
      CallApi[S](this0,"set at",List(name,value),TNothing)

    /** Sets the string value. */
    case "set string" =>
      val List(name,value) = parameters // String,String
      val curState = New[S](TJson_Builder,Map(
        TJson_Builder.field_kind -> String("string"),
        TJson_Builder.field_to_string -> value
      ))
      CallApi[S](this0,"set at",List(name,curState.expr),TNothing)(curState,pp)

    /** Sets the field the the reference to JsonBuilder. */
    case "set builder" =>
      val List(name,value) = parameters // String,Json_Builder
      val curState = Top[S](TJson_Builder)
      CallApi[S](this0,"set at",List(name,curState.expr),TNothing)(curState,pp)

    /** Sets the Picture value as a data uri. */
    case "set picture" =>
      val List(name,pic,quality) = parameters // String,Picture,Number
      val curState = Top[S](TJson_Builder)
      CallApi[S](this0,"set at",List(name,curState.expr),TNothing)(curState,pp)

    /** Sets the Sound value as a data uri. */
    case "set sound" =>
      val List(name,snd) = parameters // String,Sound
      val curState = Top[S](TJson_Builder)
      CallApi[S](this0,"set at",List(name,curState.expr),TNothing)(curState,pp)

    /** Sets the number value. */
    case "set number" =>
      val List(name,value) = parameters // String,Number
      val curState = New[S](TJson_Builder,Map(
        TJson_Builder.field_kind -> String("number"),
        TJson_Builder.field_to_number -> value
      ))
      CallApi[S](this0,"set at",List(name,curState.expr),TNothing)(curState,pp)

    /** Sets the boolean value. */
    case "set boolean" =>
      val List(name,value) = parameters // String,Boolean
      val curState = New[S](TJson_Builder,Map(
        TJson_Builder.field_kind -> String("boolean"),
        TJson_Builder.field_to_boolean -> value
      ))
      CallApi[S](this0,"set at",List(name,curState.expr),TNothing)(curState,pp)

    /** Sets the field value as null. */
    case "set field null" =>
      val List(name) = parameters // String
      CallApi[S](this0,"remove at",List(name),TNothing)

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
      Return[S](collectionAllValues[S](this0))

    /** Gets a field value as a boolean */
    case "boolean" =>
      val List(key) = parameters // String
      Return[S](Field[S](collectionAt[S](this0,key),TJson_Builder.field_to_boolean))

    /** Indicates if the key exists */
    case "contains key" =>
      val List(key) = parameters // String
      Return[S](collectionContainsKey[S](this0, key))

    /** Gets a value by name */
    case "field" =>
      super.forwardSemantics(this0,"at",parameters,returnedType)

    /** Gets a field value as a number */
    case "number" =>
      val List(key) = parameters // String
      Return[S](Field[S](collectionAt[S](this0,key),TJson_Builder.field_to_number))

    /** Gets a field value as a string */
    case "string" =>
      Return[S](Field[S](super.forwardSemantics(this0,"at",parameters,returnedType).expr, TJson_Builder.field_to_string))

    /** Gets the field value as a time */
    case "time" =>
      val List(key) = parameters // String
      Return[S](Field[S](collectionAt[S](this0,key),TJson_Builder.field_to_time))

    /** Copy current JSON object into a Json Builder so it can be modified */
    case "to json" =>
      Top[S](TJson_Object,initials = Map (
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
      
