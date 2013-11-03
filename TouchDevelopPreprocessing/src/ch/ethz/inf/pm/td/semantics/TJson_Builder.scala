
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
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

  /** Internal JSON Object */
  val field_value = new TouchField("__value",TJson_Object.typName)

  val typName = "Json Builder"
  val typ = new TouchType(typName,isSingleton = true,fields = List(field_value))

}

class TJson_Builder extends AAny {

  def getTyp = TJson_Builder.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Sets the field value. */
    case "set field" =>
      val List(name,value) = parameters // String,Json_Object
      CallApi[S](Field[S](this0,TJson_Builder.field_value),"set at",List(name,value),TNothing.typ)

    /** Sets the string value. */
    case "set string" =>
      val List(name,value) = parameters // String,String
      val curState = New[S](TJson_Object.typ,Map(
        TJson_Object.field_kind -> String("string"),
        TJson_Object.field_to_string -> value
      ))
      CallApi[S](Field[S](this0,TJson_Builder.field_value),"set at",List(name,curState.getExpression()),TNothing.typ)(curState,pp)

    /** Sets the number value. */
    case "set number" =>
      val List(name,value) = parameters // String,Number
      val curState = New[S](TJson_Object.typ,Map(
        TJson_Object.field_kind -> String("number"),
        TJson_Object.field_to_string -> value
      ))
      CallApi[S](Field[S](this0,TJson_Builder.field_value),"set at",List(name,curState.getExpression()),TNothing.typ)(curState,pp)

    /** Sets the boolean value. */
    case "set boolean" =>
      val List(name,value) = parameters // String,Boolean
      val curState = New[S](TJson_Object.typ,Map(
        TJson_Object.field_kind -> String("boolean"),
        TJson_Object.field_to_string -> value
      ))
      CallApi[S](Field[S](this0,TJson_Builder.field_value),"set at",List(name,curState.getExpression()),TNothing.typ)(curState,pp)

    /** Sets the field value as null. */
    case "set field null" =>
      val List(name) = parameters // String
      CallApi[S](Field[S](this0,TJson_Builder.field_value),"remove at",List(name),TNothing.typ)

    /** Deletes named field */
    case "remove field" =>
       val List(name) = parameters // String
       CollectionRemove[S](Field[S](this0,TJson_Builder.field_value),name)

//    /** Adds a value to the array. */
//    case "add" =>
//      val List(value) = parameters // Json_Object
//      Skip // TODO
//
//    /** Adds a null value to the array. */
//    case "add null" =>
//      val List() = parameters //
//      Skip // TODO

    /** Converts the builder into a json data structure and clears the builder. */
    case "to json" =>
      val returnObject = Field[S](this0,TJson_Builder.field_value)
      var curState = state
      curState = New[S](TJson_Object.typ)(curState,pp)
      val emptyObject = curState.getExpression()
      curState = AssignField[S](this0,TJson_Builder.field_value,emptyObject)(curState,pp)
      Return[S](returnObject)(curState,pp)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
