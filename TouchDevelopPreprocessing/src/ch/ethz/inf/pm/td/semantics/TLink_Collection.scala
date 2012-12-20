
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Link Collection
 *
 * A list of links
 *
 * @author Lucas Brutschy
 */ 

object TLink_Collection {

  val typName = "Link Collection"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TLink_Collection extends AAny {

  def getTyp = TLink_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Adds a link */
    // case "add" => 
    //   val List(value) = parameters // Link
    //   Skip;

    /** Adds many links at once */
    // case "add_many" => 
    //   val List(value) = parameters // Link_Collection
    //   Skip;

    /** Gets the i-th link */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TLink.typ))

    /** Clears the collection */
    // case "clear" => 
    //   Skip;

    /** Gets the number of elements */
    // case "count" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the number of elements */
    //   val field_count = new TouchField("count",TNumber.typ)

    /** Gets the index of the first occurence of item. Returns -1 if not found or start is out of range. */
    // case "index_of" => 
    //   val List(item,start) = parameters // Link,Number
    //   Return[S](Valid(TNumber.typ))

    /** Inserts a link at position index. Does nothing if index is out of range. */
    // case "insert_at" => 
    //   val List(index,item) = parameters // Number,Link
    //   Skip;

    /** Gets a random element from the collection. Returns invalid if the collection is empty. */
    // case "random" => 
    //   Return[S](Valid(TLink.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets a random element from the collection. Returns invalid if the collection is empty. */
    //   val field_random = new TouchField("random",TLink.typ)

    /** Removes the first occurence of the link. Returns true if removed. */
    // case "remove" => 
    //   val List(item) = parameters // Link
    //   Return[S](Valid(TBoolean.typ))

    /** Removes the link at position index. */
    // case "remove_at" => 
    //   val List(index) = parameters // Number
    //   Skip;

    /** Reverses the order of the elements. */
    // case "reverse" => 
    //   Skip;

    /** Sets the i-th link */
    // case "set_at" => 
    //   val List(index,value) = parameters // Number,Link
    //   Skip;

    // FIELDS: , field_count, field_random

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
