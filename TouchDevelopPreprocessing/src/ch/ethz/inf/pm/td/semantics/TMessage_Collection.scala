
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Message Collection
 *
 * A list of messages
 *
 * @author Lucas Brutschy
 */ 

object TMessage_Collection {

  val typName = "Message Collection"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TMessage_Collection extends AAny {

  def getTyp = TMessage_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Adds a Message */
    // case "add" => 
    //   val List(value) = parameters // Message
    //   Skip;

    /** Adds a collection of Message items */
    // case "add_many" => 
    //   val List(value) = parameters // Message_Collection
    //   Skip;

    /** Gets the i-th Message */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TMessage.typ))

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
    //   val List(item,start) = parameters // Message,Number
    //   Return[S](Valid(TNumber.typ))

    /** Inserts a link at position index. Does nothing if index is out of range. */
    // case "insert_at" => 
    //   val List(index,item) = parameters // Number,Message
    //   Skip;

    /** Gets a random element from the collection. Returns invalid if the collection is empty. */
    // case "random" => 
    //   Return[S](Valid(TMessage.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets a random element from the collection. Returns invalid if the collection is empty. */
    //   val field_random = new TouchField("random",TMessage.typ)

    /** Removes the first occurence of the message. Returns true if removed. */
    // case "remove" => 
    //   val List(item) = parameters // Message
    //   Return[S](Valid(TBoolean.typ))

    /** Removes the message at position index. */
    // case "remove_at" => 
    //   val List(index) = parameters // Number
    //   Skip;

    /** Reverses the order of the elements. */
    // case "reverse" => 
    //   Skip;

    /** Sets the i-th Message */
    // case "set_at" => 
    //   val List(index,value) = parameters // Number,Message
    //   Skip;

    /** Sorts from the newest to oldest */
    // case "sort_by_date" => 
    //   Skip;

    // FIELDS: , field_count, field_random

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
