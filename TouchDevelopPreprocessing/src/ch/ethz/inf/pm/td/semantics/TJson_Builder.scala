
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Json Builder
 *
 * A json data structure builder
 *
 * @author Lucas Brutschy
 */ 

object TJson_Builder {

  val typName = "Json Builder"
  val typ = new TouchType(typName,isSingleton = true)

}

class TJson_Builder extends AAny {

  def getTyp = TJson_Builder.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Sets the field value. */
    // case "set field" => 
    //   val List(name,value) = parameters // String,Json_Object
    //   Skip

    /** Sets the string value. */
    // case "set string" => 
    //   val List(name,value) = parameters // String,String
    //   Skip

    /** Sets the number value. */
    // case "set number" => 
    //   val List(name,value) = parameters // String,Number
    //   Skip

    /** Sets the boolean value. */
    // case "set boolean" => 
    //   val List(name,value) = parameters // String,Boolean
    //   Skip

    /** Sets the field value as null. */
    // case "set field null" => 
    //   val List(name) = parameters // String
    //   Skip

    /** Adds a value to the array. */
    // case "add" => 
    //   val List(value) = parameters // Json_Object
    //   Skip

    /** Adds a null value to the array. */
    // case "add null" => 
    //   val List() = parameters // 
    //   Skip

    /** Converts the builder into a json data structure and clears the builder. */
    // case "to json" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TJson_Object.typ)
    // DECLARATION AS FIELD: 
    //   /** Converts the builder into a json data structure and clears the builder. */
    //   val field_to_json = new TouchField("to json",TJson_Object.typ)

    // FIELDS: field_to_json

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
