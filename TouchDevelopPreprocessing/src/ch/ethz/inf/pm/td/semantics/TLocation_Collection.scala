package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{Constant, VariableIdentifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * User: lucas
 * Date: 11/26/12
 * Time: 1:16 PM
 */
object TLocation_Collection {

  /** Gets the number of elements */
  val field_count = new TouchField("count", TNumber.typ) //, 0)

  /** collection elements abstracted by a single element */
  val field_elem = new TouchField("elem", TLocation.typ, isSummaryNode = true)

  val typName = "Location_Collection"
  val typ = TouchType(typName,isSingleton = false,List(field_count,field_elem))

}

class TLocation_Collection extends Any {

  def getTyp = TLocation_Collection.typ
  def getTypeName = getTyp.name

  def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Adds a location */
    case "add" =>
      val List(value) = parameters // Location
      JoinField[S](this0,TLocation_Collection.field_elem,value)

    /** Adds many locations at once */
    case "add_many" =>
      val List(value) = parameters // Location_Collection
      JoinField[S](this0,TLocation_Collection.field_elem,Field[S](value,TLocation_Collection.field_elem))

    /** Gets the i-th geo coordinate */
    case "at" =>
      val List(index) = parameters // Number
      CheckInRangeInclusive[S](index,0,(TLocation_Collection.field_count-1),method,"index")
      Return[S](CopyOfField[S](this0,TLocation_Collection.field_elem))

    /** Clears the collection */
    case "clear" =>
      val newState = AssignField[S](this0,TLocation_Collection.field_count,toRichExpression(0))
      BottomField[S](this0,TLocation_Collection.field_elem)(newState,pp)

    /** Gets the index of the first occurence of item. Returns -1 if not found or start is out of range. */
    case "index_of" =>
      val List(item,start) = parameters // Location,Number
      // HELP: Check if start is always out of range.
      Return[S](-1 ndTo (TLocation_Collection.field_count-1))

    /** Inserts a location at position index. Does nothing if index is out of range. */
    case "insert_at" =>
      val List(index,item) = parameters // Number,Location
      CheckInRangeInclusive[S](index,0,(TLocation_Collection.field_count-1),method,"index")
      JoinField[S](this0,TLocation_Collection.field_elem,Field[S](item,TLocation_Collection.field_elem))

    /** Gets a random element from the collection. Returns invalid if the collection is empty. */
    case "random" =>
      New[S](TLocation.typ) // TODO

    /** Removes the first occurence of the location. Returns true if removed. */
    case "remove" =>
      val List(item) = parameters // Location
      Return[S](True or False)

    /** Removes the location at position index. */
    case "remove_at" =>
      val List(index) = parameters // Number
      CheckInRangeInclusive[S](index,0,(TLocation_Collection.field_count-1),method,"index")
      Skip

    /** Reverses the order of the elements. */
    case "reverse" =>
      Skip // Sorting is invariant for (size,elem) abstraction

    /** Sets the i-th geo coordinate */
    case "set_at" =>
      val List(index,value) = parameters // Number,Location
      CheckInRangeInclusive[S](index,0,(TLocation_Collection.field_count-1),method,"index")
      JoinField[S](this0,TLocation_Collection.field_elem,value)

    /** Sorts by distance to the location */
    case "sort_by_distance" =>
      val List(loc) = parameters // Location
      Skip // Sorting is invariant for (size,elem) abstraction

    case _ =>
      MatchFields[S](this0,parameters,getTyp,method)

  }
}
