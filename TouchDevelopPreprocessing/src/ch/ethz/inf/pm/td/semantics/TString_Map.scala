
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of String Map
 *
 * A map from strings to strings
 *
 * @author Lucas Brutschy
 */ 

object TString_Map {

  val typName = "String_Map"
  val typ = new TouchType(typName,isSingleton = false,List())

}

class TString_Map extends AAny {

  def getTyp = TString_Map.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the value at a given key; invalid if not found */
    // case "at" => 
    //   val List(key) = parameters // String
    //   Top[S](TString.typ)

    /** Gets the number of elements in the map */
    // case "count" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the number of elements in the map */
    //   val field_count = new TouchField("count",TNumber.typ)

    /** Gets the keys in the map */
    // case "keys" => 
    //   Top[S](TString_Collection.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the keys in the map */
    //   val field_keys = new TouchField("keys",TString_Collection.typ)

    /** Removes the value at a given key */
    // case "remove" => 
    //   val List(key) = parameters // String
    //   Skip;

    /** Sets the value at a given key; invalid if not found */
    // case "set_at" => 
    //   val List(key,value) = parameters // String,String
    //   Skip;

    /** Sets many elements at once. */
    // case "set_many" => 
    //   val List(other) = parameters // String_Map
    //   Skip;

    // FIELDS: , field_count, field_keys

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
