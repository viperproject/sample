package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, SemanticException, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichExpression, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
import RichNativeSemantics._

/**
 * A mutable collection with integer indices
 */
trait AMutable_Collection extends ALinearCollection {

  override def isImmutable = false

  /** Never used: Adds many objects at once */
  def member_add_many = ApiMember(
    name = "add many",
    paramTypes = List(ApiParam(this)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds an object */
  def member_add = ApiMember(
    name = "add",
    paramTypes = List(ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Removes all objects from the collection */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Checks if the item is in the collection */
  def member_contains = ApiMember(
    name = "contains",
    paramTypes = List(ApiParam(valueType)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the index of the first occurrence of an object. Returns -1 if not found or start is out of range. */
  def member_index_of = ApiMember(
    name = "index of",
    paramTypes = List(ApiParam(valueType), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Inserts an object at position index. Does nothing if index is out of range. */
  def member_insert_at = ApiMember(
    name = "insert at",
    paramTypes = List(ApiParam(TNumber), ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Removes the object at position index. */
  def member_remove_at = ApiMember(
    name = "remove at",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Removes the first occurence of an object. Returns true if removed. */
  def member_remove = ApiMember(
    name = "remove",
    paramTypes = List(ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Reverses the order of objects in the collection */
  def member_reverse = ApiMember(
    name = "reverse",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the object at position index. Does nothing if the index is out of range. */
  def member_set_at = ApiMember(
    name = "set at",
    paramTypes = List(ApiParam(TNumber), ApiParam(valueType)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sorts the strings in this collection */
  def member_sort = ApiMember(
    name = "sort",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add many" -> member_add_many,
    "add" -> member_add,
    "clear" -> member_clear,
    "contains" -> member_contains,
    "index of" -> member_index_of,
    "insert at" -> member_insert_at,
    "post to wall" -> member_post_to_wall,
    "random" -> member_random,
    "remove at" -> member_remove_at,
    "remove" -> member_remove,
    "reverse" -> member_reverse,
    "set at" -> member_set_at,
    "sort" -> member_sort
  )

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Adds an element */
    case "add" =>
      val List(value) = parameters // Element_Type
      val newState = collectionInsert[S](this0,collectionSize[S](this0), value)
      collectionIncreaseLength[S](this0)(newState, pp)

    /** Adds many elements at once */
    case "add many" =>
      val List(value) = parameters // Collection_Type
      Skip; // TODO: implement

    /** Clears the collection */
    case "clear" =>
      collectionClear[S](this0)

    /** Gets the index of the first occurence of item. Returns -1 if not found or start is out of range. */
    case "index of" =>
      val List(item,start) = parameters // Element_Type,Number

      if (start.getType() != TNumber)
        throw new SemanticException("This is not a linear collection " + this0)

      If[S](collectionIndexInRange[S](this0, start) && collectionContainsValue[S](this0, item) equal True , Then={
        Return[S](0 ndTo collectionSize[S](this0)-1)(_, pp)
      }, Else={
        Return[S](-1)(_, pp)
      })

    /** Inserts an element at position index. Does nothing if index is out of range. */
    case "insert at" =>
      val List(index,item) = parameters // Number,Element_Type

      if (index.getType() != TNumber)
        throw new SemanticException("This is not a linear collection " + this0)

      If[S](collectionIndexInRange[S](this0, index), Then=(state) => {
        var newState = collectionInvalidateKeys[S](this0)(state, pp)
        newState = collectionInsert[S](this0, index, item)(newState, pp)
        collectionIncreaseLength[S](this0)(newState, pp)
      }, Else=(state) => {
        state
      })

    /** Sets the i-th element */
    case "set at" =>
      val List(index, value) = parameters // Number,Element_Type

      if (index.getType() != TNumber)
        throw new SemanticException("This is not a linear collection " + this0)

      If[S](collectionIndexInRange[S](this0, index), Then=(state) => {
        //val newState = CollectionRemove[S](this0, index)(state, pp)
        //CollectionInsert[S](this0, index, value)(newState, pp)
        // FIXME: This is broken
        collectionInsert[S](this0, index, value)(state,pp)
      }, Else=(state) => {
        state
      })

    /** Removes the first occurance of the element. Returns true if removed. */
    case "remove" =>
      val List(item) = parameters // Element_Type

      //TODO
      //If[S](collectionContainsValue[S](this0, item) equal True, Then=(state) => {
      //  var newState = CollectionRemoveFirst[S](this0, item)(state, pp)
      //  newState = collectionDecreaseLength[S](this0)(newState, pp)
      //  newState = collectionInvalidateKeys(this0)(newState, pp)
      //  Return[S](True)(newState, pp)
      //}, Else= {
      //  Return[S](False)(_, pp)
      //})
      Return[S](False)(state, pp)

    /** Removes the element at position index. */
    case "remove at" =>
      val List(index) = parameters // Number

      if (index.getType() != TNumber)
        throw new SemanticException("This is not a linear collection " + this0)

      If[S](collectionIndexInRange[S](this0, index), Then=(state) => {
        var newState = CollectionRemove[S](this0, index)(state, pp)
        newState = collectionDecreaseLength[S](this0)(newState, pp)
        collectionInvalidateKeys[S](this0)(newState, pp)
      }, Else={
        CollectionRemove[S](this0, index)(_, pp)
      })

    /** Reverses the order of the elements. */
    case "reverse" =>
      collectionInvalidateKeys[S](this0)

    case "sort" =>
      collectionInvalidateKeys[S](this0)

    case "contains" =>
      val List(item) = parameters
      If[S](collectionAllKeys[S](this0) equal item, Then={
        Return[S](True)(_,pp)
      }, Else={
        Return[S](False)(_,pp)
      })

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)
  }
}