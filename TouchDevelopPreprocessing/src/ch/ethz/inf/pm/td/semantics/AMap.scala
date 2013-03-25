//package ch.ethz.inf.pm.td.semantics
//
//import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
//import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
//import ch.ethz.inf.pm.td.compiler.TouchCollection
//import RichNativeSemantics._
//
///**
// * A mutable collection with integer indices
// */
//abstract class AMap extends ACollection {
//
//  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
//                                     (implicit pp:ProgramPoint,state:S):S = method match {
//
//    /** Gets the element with key index */
//    case "at" =>
//      val List(index) = parameters // Key_Type
//      Return[S](CollectionAt[S](this0,index))
//
//    /** Gets the i-th element */
//    case "at_index" =>
//      val List(index) = parameters // Number
//      CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
//      Return[S](CollectionAt[S](this0,index)) // TODO: Get the i-th key
//
//    /** Returns the length of the collection*/
//    case "count" =>
//      Return[S](CollectionSize[S](this0))
//
//    /** Removes the item at key key. */
//    case "remove" =>
//      val List(k) = parameters // Key_Type
//      // Check if contained
//      CollectionRemove[S](this0,k)
//
//    /** Sets the element at key k */
//    case "set_at" =>
//      val List(k,value) = parameters // Key_Type,Element_Type
//      // Check if contained, then update or insert
//      CollectionUpdate[S](this0,index,value)
//
//    /** Sets the element at key k */
//    case "set_many" =>
//      val List(k,value) = parameters // Number,Element_Type
//      // Check if contained, then update or insert
//      CollectionUpdate[S](this0,index,value)
//
//    case _ =>
//      super.forwardSemantics(this0,method,parameters)
//
//  }
//}
