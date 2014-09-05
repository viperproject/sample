
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Collection
 *
 * A collection of objects
 *
 * @author Lucas Brutschy
 */ 

object TCollection extends AAny {

  lazy val typeName = TypeName("Collection")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Adds many objects at once */
    // case "add many" => 
    //   val List(items) = parameters // {"g":"Collection","a":["T"]}
    //   Skip

    /** Adds an object */
    // case "add" => 
    //   val List(item) = parameters // T
    //   Skip

    /** Gets the object at position index. Returns invalid if index is out of range */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   TopWithInvalid[S](TT)
    // DECLARATION AS FIELD: 
    //   /** Gets the object at position index. Returns invalid if index is out of range */
    //   lazy val field_at = new TouchField("at",TT.typeName)

    /** Computes the average of the key of the elements in the collection */
    // case "avg of" => 
    //   val List(key) = parameters // {"g":"Number_Converter","a":["T"]}
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Computes the average of the key of the elements in the collection */
    //   lazy val field_avg_of = new TouchField("avg of",TNumber.typeName)

    /** Computes the average of the values */
    // case "avg" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Computes the average of the values */
    //   lazy val field_avg = new TouchField("avg",TNumber.typeName)

    /** Removes all objects from the collection */
    // case "clear" => 
    //   val List() = parameters // 
    //   Skip

    /** Checks if the item is in the collection */
    // case "contains" => 
    //   val List(item) = parameters // T
    //   TopWithInvalid[S](TBoolean)
    // DECLARATION AS FIELD: 
    //   /** Checks if the item is in the collection */
    //   lazy val field_contains = new TouchField("contains",TBoolean.typeName)

    /** Gets the number of objects. */
    // case "count" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Gets the number of objects. */
    //   lazy val field_count = new TouchField("count",TNumber.typeName)

    /** Imports a JSON representation of the contents. */
    // case "from json" => 
    //   val List(jobj) = parameters // Json_Object
    //   Skip

    /** Gets the index of the first occurrence of an object. Returns -1 if not found or start is out of range. */
    // case "index of" => 
    //   val List(item,start) = parameters // T,Number
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Gets the index of the first occurrence of an object. Returns -1 if not found or start is out of range. */
    //   lazy val field_index_of = new TouchField("index of",TNumber.typeName)

    /** Inserts an object at position index. Does nothing if index is out of range. */
    // case "insert at" => 
    //   val List(index,item) = parameters // Number,T
    //   Skip

    /** Concatenates the separator and items into a string */
    // case "join" => 
    //   val List(separator) = parameters // String
    //   TopWithInvalid[S](TString)
    // DECLARATION AS FIELD: 
    //   /** Concatenates the separator and items into a string */
    //   lazy val field_join = new TouchField("join",TString.typeName)

    /** [**beta**] Applies `converter` on all elements of the input collection and returns a collection of results */
    // case "map to" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TUnfinished_Type)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Applies `converter` on all elements of the input collection and returns a collection of results */
    //   lazy val field_map_to = new TouchField("map to",TUnfinished_Type.typeName)

    /** Computes the maximum of the key of the elements in the collection */
    // case "max of" => 
    //   val List(key) = parameters // {"g":"Number_Converter","a":["T"]}
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Computes the maximum of the key of the elements in the collection */
    //   lazy val field_max_of = new TouchField("max of",TNumber.typeName)

    /** Computes the maximum of the values */
    // case "max" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Computes the maximum of the values */
    //   lazy val field_max = new TouchField("max",TNumber.typeName)

    /** Computes the minimum of the key of the elements in the collection */
    // case "min of" => 
    //   val List(key) = parameters // {"g":"Number_Converter","a":["T"]}
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Computes the minimum of the key of the elements in the collection */
    //   lazy val field_min_of = new TouchField("min of",TNumber.typeName)

    /** Computes the minimum of the values */
    // case "min" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Computes the minimum of the values */
    //   lazy val field_min = new TouchField("min",TNumber.typeName)

    /** Returns a collection sorted using specified comparison key */
    // case "ordered by string" => 
    //   val List(key) = parameters // {"g":"String_Converter","a":["T"]}
    //   TopWithInvalid[S](T{"g":"Collection","a":["T"]})
    // DECLARATION AS FIELD: 
    //   /** Returns a collection sorted using specified comparison key */
    //   lazy val field_ordered_by_string = new TouchField("ordered by string",T{"g":"Collection","a":["T"]}.typeName)

    /** Returns a collection sorted using specified comparison key */
    // case "ordered by" => 
    //   val List(key) = parameters // {"g":"Number_Converter","a":["T"]}
    //   TopWithInvalid[S](T{"g":"Collection","a":["T"]})
    // DECLARATION AS FIELD: 
    //   /** Returns a collection sorted using specified comparison key */
    //   lazy val field_ordered_by = new TouchField("ordered by",T{"g":"Collection","a":["T"]}.typeName)

    /** Ask user to pick an entry from this collection */
    // case "pick entry" => 
    //   val List(text) = parameters // String
    //   TopWithInvalid[S](TT)
    // DECLARATION AS FIELD: 
    //   /** Ask user to pick an entry from this collection */
    //   lazy val field_pick_entry = new TouchField("pick entry",TT.typeName)

    /** Gets a random object from the collection. Returns invalid if the collection is empty. */
    // case "random" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TT)
    // DECLARATION AS FIELD: 
    //   /** Gets a random object from the collection. Returns invalid if the collection is empty. */
    //   lazy val field_random = new TouchField("random",TT.typeName)

    /** Removes the object at position index. */
    // case "remove at" => 
    //   val List(index) = parameters // Number
    //   Skip

    /** Removes the first occurence of an object. Returns true if removed. */
    // case "remove" => 
    //   val List(item) = parameters // T
    //   TopWithInvalid[S](TBoolean)
    // DECLARATION AS FIELD: 
    //   /** Removes the first occurence of an object. Returns true if removed. */
    //   lazy val field_remove = new TouchField("remove",TBoolean.typeName)

    /** Reverses the order of objects in the collection */
    // case "reverse" => 
    //   val List() = parameters // 
    //   Skip

    /** Sets the object at position index. Does nothing if the index is out of range. */
    // case "set at" => 
    //   val List(index,item) = parameters // Number,T
    //   Skip

    /** Returns a slice of the collection starting at `start`, and ends at, but does not include, the `end`. */
    // case "slice" => 
    //   val List(start,end) = parameters // Number,Number
    //   TopWithInvalid[S](T{"g":"Collection","a":["T"]})
    // DECLARATION AS FIELD: 
    //   /** Returns a slice of the collection starting at `start`, and ends at, but does not include, the `end`. */
    //   lazy val field_slice = new TouchField("slice",T{"g":"Collection","a":["T"]}.typeName)

    /** Sorts from the newest to oldest */
    // case "sort by date" => 
    //   val List() = parameters // 
    //   Skip

    /** Sorts the places by distance to the location */
    // case "sort by distance" => 
    //   val List(loc) = parameters // Location
    //   Skip

    /** Sorts the strings in this collection */
    // case "sort" => 
    //   val List() = parameters // 
    //   Skip

    /** Returns a collection sorted using specified `comparison` function */
    // case "sorted" => 
    //   val List(comparison) = parameters // {"g":"Comparison","a":["T"]}
    //   TopWithInvalid[S](T{"g":"Collection","a":["T"]})
    // DECLARATION AS FIELD: 
    //   /** Returns a collection sorted using specified `comparison` function */
    //   lazy val field_sorted = new TouchField("sorted",T{"g":"Collection","a":["T"]}.typeName)

    /** Computes the sum of the key of the elements in the collection */
    // case "sum of" => 
    //   val List(key) = parameters // {"g":"Number_Converter","a":["T"]}
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Computes the sum of the key of the elements in the collection */
    //   lazy val field_sum_of = new TouchField("sum of",TNumber.typeName)

    /** Computes the sum of the values */
    // case "sum" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Computes the sum of the values */
    //   lazy val field_sum = new TouchField("sum",TNumber.typeName)

    /** Returns a collection with the `count` first elements if any. */
    // case "take" => 
    //   val List(count) = parameters // Number
    //   TopWithInvalid[S](T{"g":"Collection","a":["T"]})
    // DECLARATION AS FIELD: 
    //   /** Returns a collection with the `count` first elements if any. */
    //   lazy val field_take = new TouchField("take",T{"g":"Collection","a":["T"]}.typeName)

    /** Exports a JSON representation of the contents. */
    // case "to json" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TJson_Object)
    // DECLARATION AS FIELD: 
    //   /** Exports a JSON representation of the contents. */
    //   lazy val field_to_json = new TouchField("to json",TJson_Object.typeName)

    /** Returns a collections of elements that satisfy the filter `condition` */
    // case "where" => 
    //   val List(condition) = parameters // {"g":"Predicate","a":["T"]}
    //   TopWithInvalid[S](T{"g":"Collection","a":["T"]})
    // DECLARATION AS FIELD: 
    //   /** Returns a collections of elements that satisfy the filter `condition` */
    //   lazy val field_where = new TouchField("where",T{"g":"Collection","a":["T"]}.typeName)

    // FIELDS: field_at, field_avg_of, field_avg, field_contains, field_count, field_index_of, field_join, field_map_to, field_max_of, field_max, field_min_of, field_min, field_ordered_by_string, field_ordered_by, field_pick_entry, field_random, field_remove, field_slice, field_sorted, field_sum_of, field_sum, field_take, field_to_json, field_where

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
