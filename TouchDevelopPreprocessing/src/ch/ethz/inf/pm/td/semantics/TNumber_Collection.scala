
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Number Collection
 *
 * A collection of numbers
 *
 * @author Lucas Brutschy
 */ 

object TNumber_Collection {

  val typName = "Number Collection"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TNumber_Collection extends AAny {

  def getTyp = TNumber_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Adds a number at the end of the collection */
    // case "add" => 
    //   val List(item) = parameters // Number
    //   Skip;

    /** Adds many numbers at once */
    // case "add_many" => 
    //   val List(items) = parameters // Number_Collection
    //   Skip;

    /** Gets the number at position index. Returns invalid if index is out of range */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TNumber.typ))

    /** Computes the average of the values */
    // case "avg" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Computes the average of the values */
    //   val field_avg = new TouchField("avg",TNumber.typ)

    /** Clears the numbers */
    // case "clear" => 
    //   Skip;

    /** Indicates if the collection contains the item */
    // case "contains" => 
    //   val List(item) = parameters // Number
    //   Return[S](Valid(TBoolean.typ))

    /** Gets the number of items */
    // case "count" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the number of items */
    //   val field_count = new TouchField("count",TNumber.typ)

    /** Gets the index of the first occurence of a number. Returns -1 if not found or start is out of range. */
    // case "index_of" => 
    //   val List(item,start) = parameters // Number,Number
    //   Return[S](Valid(TNumber.typ))

    /** Inserts a double at position index. Does nothing if index is out of range. */
    // case "insert_at" => 
    //   val List(index,item) = parameters // Number,Number
    //   Skip;

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

    /** Gets a random element from the collection. Returns invalid if the collection is empty. */
    // case "random" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets a random element from the collection. Returns invalid if the collection is empty. */
    //   val field_random = new TouchField("random",TNumber.typ)

    /** Removes the first occurence of a number. Returns true if removed. */
    // case "remove" => 
    //   val List(item) = parameters // Number
    //   Return[S](Valid(TBoolean.typ))

    /** Removes the number at position index. */
    // case "remove_at" => 
    //   val List(index) = parameters // Number
    //   Skip;

    /** Reverses the items */
    // case "reverse" => 
    //   Skip;

    /** Sets the number at position index. Does nothing if the index is out of range. */
    // case "set_at" => 
    //   val List(index,item) = parameters // Number,Number
    //   Skip;

    /** Sorts the numbers in this collection */
    // case "sort" => 
    //   Skip;

    /** Computes the sum of the values */
    // case "sum" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Computes the sum of the values */
    //   val field_sum = new TouchField("sum",TNumber.typ)

    // FIELDS: , field_avg, field_count, field_max, field_min, field_random, field_sum

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
