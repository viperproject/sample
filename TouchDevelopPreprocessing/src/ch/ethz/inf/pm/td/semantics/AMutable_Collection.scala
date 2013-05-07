package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{TouchType, TouchCollection}
import RichNativeSemantics._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalAnalysisConstants
import ch.ethz.inf.pm.sample.Reporter

/**
 * A mutable collection with integer indices
 */
abstract class AMutable_Collection extends ACollection {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Adds an element */
    case "add" =>
      val List(value) = parameters // Element_Type
      CollectionInsert[S](this0,CollectionSize[S](this0)-1,value)

    /** Adds many elements at once */
    case "add many" =>
      val List(value) = parameters // Collection_Type
      Skip; // TODO

    /** Clears the collection */
    case "clear" =>
      Skip; // TODO

    /** Gets the index of the first occurence of item. Returns -1 if not found or start is out of range. */
    case "index of" =>
      val List(item,start) = parameters // Element_Type,Number
      // HELP: Check if start is always out of range.
      Return[S](-1 ndTo (CollectionSize[S](this0)-1))

    /** Inserts an element at position index. Does nothing if index is out of range. */
    case "insert at" =>
      val List(index,item) = parameters // Number,Element_Type

      if (index.getType().getName() == "Number")
        CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
      else Reporter.hasImprecision("This map access is not checked",pp)

      CollectionInsert[S](this0,index,item)

    /** Gets a random element from the collection. Returns invalid if the collection is empty. */
    case "random" =>
      Return[S](Invalid(getTyp.asInstanceOf[TouchCollection].getValueType)) // TODO

    /** Removes the first occurence of the element. Returns true if removed. */
    case "remove" =>
      val List(item) = parameters // Element_Type
      Return[S](True or False) // TODO

    /** Removes the element at position index. */
    case "remove at" =>
      val List(index) = parameters // Number

      if (index.getType().getName() == "Number")
        CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
      else Reporter.hasImprecision("This map access is not checked",pp)

      CollectionRemove[S](this0,index)

    /** Reverses the order of the elements. */
    case "reverse" =>
      Skip // Sorting is invariant for (size,elem) abstraction

    /** Sets the i-th element */
    case "set at" =>
      val List(index,value) = parameters // Number,Element_Type

      if (index.getType().getName() == "Number" && !this0.getType().toString.contains("Map"))
        CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
      else Reporter.reportImprecision("This map access is not checked",pp)

      CollectionUpdate[S](this0,index,value)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
