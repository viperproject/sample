
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Number Map
 *
 * A map of numbers to numbers
 *
 * @author Lucas Brutschy
 */ 

object TNumber_Map {

  val typName = "Number Map"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TNumber_Map extends AAny {

  def getTyp = TNumber_Map.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the element at index. Index may be any floating-point value. */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TNumber.typ))

    /** Computes the average of the values */
    // case "avg" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Computes the average of the values */
    //   val field_avg = new TouchField("avg",TNumber.typ)

    /** Gets the number of elements */
    // case "count" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the number of elements */
    //   val field_count = new TouchField("count",TNumber.typ)

    /** Computes the maximum of the values */
    // case "max" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Computes the maximum of the values */
    //   val field_max = new TouchField("max",TNumber.typ)

    /** Computes the minimum of the values */
    // case "min" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Computes the minimum of the values */
    //   val field_min = new TouchField("min",TNumber.typ)

    /** Removes the value at a given index */
    // case "remove" => 
    //   val List(index) = parameters // Number
    //   Skip;

    /** Sets the element at index. Index may be any floating-point value. */
    // case "set_at" => 
    //   val List(index,value) = parameters // Number,Number
    //   Skip;

    /** Sets many elements at once. */
    // case "set_many" => 
    //   val List(numbers) = parameters // Number_Map
    //   Skip;

    /** Extracts the elements at indices between start (inclusive) and end (non-inclusive). */
    // case "slice" => 
    //   val List(start,end) = parameters // Number,Number
    //   Return[S](Valid(TNumber_Map.typ))

    /** Computes the sum of the values */
    // case "sum" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Computes the sum of the values */
    //   val field_sum = new TouchField("sum",TNumber.typ)

    /** Updates any display of this map */
    // case "update_on_wall" => 
    //   Skip;

    // FIELDS: , field_avg, field_count, field_max, field_min, field_sum

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
