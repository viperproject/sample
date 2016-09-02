package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{RefType, SilverAnalysisRunner}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging

/** Heap node.
  *
  * @param id the unique identifier of the heap node
  * @author Caterina Urban
  */
case class HeapNode(id: List[Identifier] = List.empty, pp: ProgramPoint = DummyProgramPoint) extends Identifier.HeapIdentifier
{
  override def getName: String = "0\"" + id.map(_.getName).mkString(".") + "\""
  override def equals(o: Any) = o match {
    case that: HeapNode => (this.getName equals that.getName) && (this.pp equals that.pp)
    case _ => false
  }
  override def hashCode = getName.hashCode
  override def getField: Option[String] = None
  override def representsSingleVariable: Boolean = true
  override def typ: Type = DummyRefType
  override def toString: String = getName
}

/** Unique heap summary node.
  *
  * @author Caterina Urban
  */
object SummaryHeapNode extends HeapNode(List.empty) {
  override def getName: String = "Σ"
  override def representsSingleVariable: Boolean = false
  override def toString: String = "Σ"
}

/** Null heap node.
  *
  * @author Caterina Urban
  */
object NullHeapNode extends HeapNode(List.empty) {
  override def getName: String = "null"
  override def representsSingleVariable: Boolean = false
  override def toString: String = "null"
}

/** Unknown heap node.
  *
  * @author Caterina Urban
  */
object UnknownHeapNode extends HeapNode(List.empty) {
  override def getName: String = "?"
  override def representsSingleVariable: Boolean = false
  override def toString: String = "?"
}

/**
  * @author Jerome Dohrau, Caterina Urban
  */
trait AliasAnalysisState[T <: AliasAnalysisState[T]]
  extends SimpleState[T]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {
  this: T =>

  type AccessPath = List[Identifier]

  // set of fields declared in the program
  def fields: Set[(Type, String)]

  // current program point
  def currentPP: ProgramPoint

  // true = materialization allowed; false = materialization not allowed
  def materialization: Boolean

  // result of the previous statement
  def result: ExpressionSet

  // map from Ref variables to heap objects
  def mayStore: Map[VariableIdentifier,Set[HeapNode]]
  def mustStore: Map[VariableIdentifier,Set[HeapNode]]

  // map from heap objects to a map from Ref fields to heap objects
  def mayHeap: Map[HeapNode,Map[String,Set[HeapNode]]]
  def mustHeap: Map[HeapNode,Map[String,Set[HeapNode]]]

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  override def assignField(obj: Expression, field: String, right: Expression): T = {
    logger.trace("*** ----------------assignField(" + obj.toString + "; " + field.toString + "; " + right.toString + ")")

    obj match {
      case AccessPathIdentifier(path) =>
        if (obj.typ.isObject) { // the assigned field is a Ref
          right match {
            case AccessPathIdentifier(right) => // e.g., `x.f := y.g`
              val mayObjR = mayEvaluateReceiver(right) // set of (right) receivers
              val mustObjR = mustEvaluateReceiver(right)
              val mayR = mayObjR.foldLeft(Set.empty[HeapNode])(
                (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
              ) // set of heap nodes pointed to by the right path
              val mustR = mustObjR.foldLeft(Set.empty[HeapNode])(
                (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
              )
              val mayRcvSet = mayEvaluateReceiver(path) - NullHeapNode // set of path receivers (excluding null)
              val mustRcvSet = mustEvaluateReceiver(path) - NullHeapNode
              // update the heap
              val mayHeapMap = mayRcvSet.foldLeft(mayHeap)((map, node) => {
                if (node.representsSingleVariable) { // strong update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field -> mayR)))
                } else { // weak update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field ->
                    (map.getOrElse(node,Map.empty).getOrElse(field,Set.empty) ++ mayR))))
                }
              })
              val mustHeapMap = mustRcvSet.foldLeft(mustHeap)((map, node) => {
                map + (node -> (map.getOrElse(node,Map.empty) + (field -> mayR)))
              })
              // return the current state with updated heap
              copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()

            case right: Constant => // e.g., `x.f := null`
              val mayRcvSet = mayEvaluateReceiver(path) - NullHeapNode // set of path receivers (excluding null)
              val mustRcvSet = mustEvaluateReceiver(path) - NullHeapNode
              // update the heap
              val mayHeapMap = mayRcvSet.foldLeft(mayHeap)((map, node) => {
                if (node.representsSingleVariable) { // strong update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field -> Set[HeapNode](NullHeapNode))))
                } else { // weak update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field ->
                    (map.getOrElse(node,Map.empty).getOrElse(field,Set.empty) ++ Set[HeapNode](NullHeapNode)))))
                }
              })
              val mustHeapMap = mustRcvSet.foldLeft(mustHeap)((map, node) => {
                map + (node -> (map.getOrElse(node,Map.empty) + (field -> Set[HeapNode](NullHeapNode)))) // strong update
              })
              // return the current state with updated heap
              copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x.f := y`
              val mayR = mayStore.getOrElse(right, Set.empty) // set heap nodes pointed to by the right identifier
              val mustR = mustStore.getOrElse(right, Set.empty)
              val mayRcvSet = mayEvaluateReceiver(path) - NullHeapNode // set of path receivers (excluding null)
              val mustRcvSet = mustEvaluateReceiver(path) - NullHeapNode
              // update the heap
              val mayHeapMap = mayRcvSet.foldLeft(mayHeap)((map, node) => {
                if (node.representsSingleVariable) { // strong update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field -> mayR)))
                } else { // weak update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field ->
                    (map.getOrElse(node,Map.empty).getOrElse(field,Set.empty) ++ mayR))))
                }
              })
              val mustHeapMap = mustRcvSet.foldLeft(mustHeap)((map, node) => {
                map + (node -> (map.getOrElse(node,Map.empty) + (field -> mustR))) // strong update
              })
              // return the current state with updated heap
              copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()

            case _ => throw new NotImplementedError("A field assignment implementation is missing.")
          }
        } else this  // the assigned field is not a Ref
      case _ => throw new IllegalArgumentException("A field assignment must occur via an AccessPathIdentifier.")
    }
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  override def assignVariable(x: Expression, right: Expression): T = {
    logger.trace("*** ----------------assignVariable(" + x.toString + "; " + right.toString + ")")

    x match {
      case x: VariableIdentifier =>
        if (x.typ.isObject) { // the assigned variable is a Ref
          right match {
            case AccessPathIdentifier(right) => // e.g., `x := y.g`
              val mayObjR = mayEvaluateReceiver(right) // set of (may right) receivers
              val mustObjR = mustEvaluateReceiver(right) // set of (must right) receivers
              val mayR = mayObjR.foldLeft(Set.empty[HeapNode])(
                (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
              ) // set of heap nodes that may be pointed to by the right path
              val mustR = mustObjR.foldLeft(Set.empty[HeapNode])(
                (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
              ) // set of heap nodes that must be pointed to by the right path
              // return the current state with updated store
              copy(mayStore = mayStore + (x -> mayR), mustStore = mustStore + (x -> mustR)).pruneUnreachableHeap()

            case right: HeapNode => // e.g., `x = new()`
              // create fresh heap node to update the path associated with right
              val fresh = HeapNode(List[Identifier](x),right.pp)
              // add x -> right to store map
              val mayStoreMap = mayStore + (x -> Set[HeapNode](fresh))
              val mustStoreMap = mustStore + (x -> Set[HeapNode](fresh))
              // update the heap to update the path associated with right
              var mayHeapMap = mayHeap - right
              mayHeapMap = mayHeapMap + (fresh -> (mayHeap.getOrElse(right, Map.empty)))
              var mustHeapMap = mustHeap - right
              mustHeapMap = mustHeapMap + (fresh -> (mustHeap.getOrElse(right, Map.empty)))
              // return the current state with updated store and heap
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap, mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x := y`
              // add x -> store[right] to store map
              val mayStoreMap = mayStore + (x -> this.mayStore.getOrElse(right, Set[HeapNode](NullHeapNode)))
              val mustStoreMap = mustStore + (x -> this.mustStore.getOrElse(right, Set[HeapNode]()))
              // return the current state with updated store
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap).pruneUnreachableHeap()

            case _ => throw new NotImplementedError("A variable assignment implementation is missing.")
          }
        } else this // the assigned variable is not a Ref
      case _ => throw new IllegalArgumentException("A variable assignment must occur via a VariableIdentifier.")
    }
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): T = {
    logger.trace("*** ----------------assume(" + cond.toString + ")")

    cond match {
      case Constant("true", _, _) => this // True
      case Constant("false", _, _) => this.bottom() // False
      case cond: Identifier => this // Identifier
      case cond: BinaryArithmeticExpression => this // BinaryArithmeticExpression
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, typ) => // BinaryBooleanExpression
        this.assume(left).assume(right)
      case BinaryBooleanExpression(left, right, BooleanOperator.||, typ) => // BinaryBooleanExpression
        this.assume(left) lub this.assume(right)
      case cond: NegatedBooleanExpression => // NegatedBooleanExpression
        cond.exp match {
          case Constant("true", _, _) => this.bottom() // True
          case Constant("false", _, _) => this // False
          case id: Identifier => this // Identifier
          case BinaryArithmeticExpression(left, right, op, typ) => this // BinaryArithmeticExpression
          case BinaryBooleanExpression(left, right, op, typ) => // BinaryBooleanExpression
            val nleft = NegatedBooleanExpression(left)
            val nright = NegatedBooleanExpression(right)
            val nop = op match {
              case BooleanOperator.&& => BooleanOperator.||
              case BooleanOperator.|| => BooleanOperator.&&
            }
            this.assume(BinaryBooleanExpression(nleft, nright, nop, typ))
          case NegatedBooleanExpression(exp) => this.assume(exp) // NegatedBooleanExpression
          case ReferenceComparisonExpression(left, right, op, typ) => // ReferenceComparisonExpression
            val nop = op match {
              case ArithmeticOperator.== => ArithmeticOperator.!=
              case ArithmeticOperator.!= => ArithmeticOperator.==
            }
            this.assume(ReferenceComparisonExpression(left, right, nop, typ))
          case _ => throw new NotImplementedError("An assumeNegatedBooleanExpression implementation for "
            + cond.exp.getClass.getSimpleName + " is missing.")
        }
      case ReferenceComparisonExpression(left, right, ArithmeticOperator.==, typ) =>
        (left, right) match {
          case (Constant("null",_,_), right: VariableIdentifier) => // e.g., null == y
            // set of heap nodes that may/must be pointed to by the right identifier
            val mayR = mayStore.getOrElse(right, Set.empty)
            val mustR = mustStore.getOrElse(right, Set.empty)
            if (mayR.contains(NullHeapNode) && mustR.contains(NullHeapNode)) {
              // return the current state with updated store
              val mayStoreMap = mayStore + (right -> Set[HeapNode](NullHeapNode))
              val mustStoreMap = mustStore + (right -> Set[HeapNode](NullHeapNode))
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap)
            } else this.bottom() // there is no common heap node
          case (Constant("null",_,_), AccessPathIdentifier(right)) => // e.g., null == y.f
            val mayObjR = mayEvaluateReceiver(right) // set of (may right) receivers
            val mustObjR = mustEvaluateReceiver(right) // set of (must right) receivers
            val mayR = mayObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the right path
            val mustR = mustObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the right path
            if (mayR.contains(NullHeapNode) && mustR.contains(NullHeapNode)) {
              // return the current state with updated heap
              val mayHeapMap = mayObjR.foldLeft(mayHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> Set(NullHeapNode))))
              }
              val mustHeapMap = mustObjR.foldLeft(mustHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> Set(NullHeapNode))))
              }
              copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
            } else this.bottom() // there is no common heap node
          case (left: VariableIdentifier, Constant("null",_,_)) => // e.g., x == null
            // set of heap nodes that may/must be pointed to by the left identifier
            val mayL = mayStore.getOrElse(left, Set.empty)
            val mustL = mustStore.getOrElse(left, Set.empty)
            if (mayL.contains(NullHeapNode) && mustL.contains(NullHeapNode)) {
              // return the current state with updated store
              val mayStoreMap = mayStore + (left -> Set[HeapNode](NullHeapNode))
              val mustStoreMap = mustStore + (left -> Set[HeapNode](NullHeapNode))
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap)
            } else this.bottom() // there is no common heap node
          case (left: VariableIdentifier, right: VariableIdentifier) => // e.g, x == y
            // set of heap nodes that may/must be pointed to by the left identifier
            val mayL = mayStore.getOrElse(left, Set.empty)
            val mustL = mustStore.getOrElse(left, Set.empty)
            // set of heap nodes that may/must be pointed to by the right identifier
            val mayR = mayStore.getOrElse(right, Set.empty)
            val mustR = mustStore.getOrElse(right, Set.empty)
            val mayIntersection = mayL intersect mayR
            val mustIntersection = mustL intersect mustR
            if (mayIntersection.isEmpty || mustIntersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common heap node
              // return the current state with updated store
              val mayStoreMap = mayStore + (left -> mayIntersection, right -> mayIntersection)
              val mustStoreMap = mustStore + (left -> mustIntersection, right -> mustIntersection)
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap).pruneUnreachableHeap()
            }
          case (left: VariableIdentifier, AccessPathIdentifier(right)) => // e.g., x == y.f
            // set heap nodes pointed to by the left identifier
            val mayL = mayStore.getOrElse(left, Set.empty)
            val mustL = mustStore.getOrElse(left, Set.empty)
            val mayObjR = mayEvaluateReceiver(right) // set of (may right) receivers
            val mustObjR = mustEvaluateReceiver(right) // set of (must right) receivers
            val mayR = mayObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the right path
            val mustR = mustObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the right path
            val mayIntersection = mayL intersect mayR
            val mustIntersection = mustL intersect mustR
            if (mayIntersection.isEmpty || mustIntersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
              // return the current state with updated store and heap
              val mayStoreMap = mayStore + (left -> mayIntersection)
              val mustStoreMap = mustStore + (left -> mustIntersection)
              val mayHeapMap = mayObjR.foldLeft(mayHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> mayIntersection)))
              }
              val mustHeapMap = mustObjR.foldLeft(mustHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> mustIntersection)))
              }
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap, mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), Constant("null",_,_)) => // e.g., x.f == null
            val mayObjL = mayEvaluateReceiver(left) // set of (may left) receivers
            val mustObjL = mustEvaluateReceiver(left) // set of (must left) receivers
            val mayL = mayObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the left path
            val mustL = mustObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the left path
            if (mayL.contains(NullHeapNode) && mustL.contains(NullHeapNode)) {
              // return the current state with updated heap
              val mayHeapMap = mayObjL.foldLeft(mayHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> Set(NullHeapNode))))
              }
              val mustHeapMap = mustObjL.foldLeft(mustHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> Set(NullHeapNode))))
              }
              copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
            } else this.bottom() // there is no common heap node
          case (AccessPathIdentifier(left), right: VariableIdentifier) => // e.g., x.f == y
            val mayObjL = mayEvaluateReceiver(left) // set of (may left) receivers
            val mustObjL = mustEvaluateReceiver(left) // set of (must left) receivers
            val mayL = mayObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the left path
            val mustL = mustObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the left path
            // set of heap nodes that may/must be pointed to by the right identifier
            val mayR = mayStore.getOrElse(right, Set.empty)
            val mustR = mustStore.getOrElse(right, Set.empty)
            val mayIntersection = mayL intersect mayR
            val mustIntersection = mustL intersect mustR
            if (mayIntersection.isEmpty || mustIntersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
              // return the current state with updated store and heap
              val mayHeapMap = mayObjL.foldLeft(mayHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> mayIntersection)))
              }
              val mustHeapMap = mustObjL.foldLeft(mustHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> mustIntersection)))
              }
              val mayStoreMap = mayStore + (right -> mayIntersection)
              val mustStoreMap = mustStore + (right -> mustIntersection)
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap, mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), AccessPathIdentifier(right)) => // e.g., x.f == y.f
            val mayObjL = mayEvaluateReceiver(left) // set of (may left) receivers
            val mustObjL = mustEvaluateReceiver(left) // set of (must left) receivers
            val mayObjR = mayEvaluateReceiver(right) // set of (may right) receivers
            val mustObjR = mustEvaluateReceiver(right) // set of (must right) receivers
            val mayL = mayObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the left path
            val mustL = mustObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the left path
            val mayR = mayObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the right path
            val mustR = mustObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the right path
            val mayIntersection = mayL intersect mayR
            val mustIntersection = mustL intersect mustR
            if (mayIntersection.isEmpty || mustIntersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
              // return the current state with updated heap
              var mayHeapMap = mayObjL.foldLeft(mayHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> mayIntersection)))
              }
              mayHeapMap = mayObjR.foldLeft(mayHeapMap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> mayIntersection)))
              }
              var mustHeapMap = mustObjL.foldLeft(mustHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> mustIntersection)))
              }
              mustHeapMap = mustObjR.foldLeft(mustHeapMap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> mustIntersection)))
              }
              copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
            }
        }
      case ReferenceComparisonExpression(left, right, ArithmeticOperator.!=, typ) =>
        (left, right) match {
          case (Constant("null", _, _), right: VariableIdentifier) => // e.g., null != y
            // set of heap nodes that may/must be pointed to by the right identifier
            val mayR = mayStore.getOrElse(right, Set.empty)
            val mustR = mustStore.getOrElse(right, Set.empty)
            val mayIntersection = Set[HeapNode](NullHeapNode) intersect mayR
            val mustIntersection = Set[HeapNode](NullHeapNode) intersect mustR
            val mayDifference = mayR diff mayIntersection
            val mustDifference = mustR diff mustIntersection
            if ((mayDifference.isEmpty && mayIntersection.size == 1) || (mustDifference.isEmpty && mustIntersection.size == 1))
              this.bottom() // there is no different heap node
            else { // there is at least one different heap node
              // return the current state with updated store
              val mayStoreMap = mayStore + (right -> mayDifference)
              val mustStoreMap = mustStore + (right -> mustDifference)
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap).pruneUnreachableHeap()
            }
          case (Constant("null", _, _), AccessPathIdentifier(right)) => // e.g., null != y.f
            val mayObjR = mayEvaluateReceiver(right) // set of (may right) receivers
            val mustObjR = mustEvaluateReceiver(right) // set of (must right) receivers
            val mayR = mayObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the right path
            val mustR = mustObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the right path
            val mayIntersection = Set[HeapNode](NullHeapNode) intersect mayR
            val mustIntersection = Set[HeapNode](NullHeapNode) intersect mustR
            val mayDifference = mayR diff mayIntersection
            val mustDifference = mustR diff mustIntersection
            if ((mayDifference.isEmpty && mayIntersection.size == 1) || (mustDifference.isEmpty && mustIntersection.size == 1))
              this.bottom() // there is no different heap node
            else { // there is at least one different heap node
              // return the current state with updated heap
              val mayHeapMap = mayObjR.foldLeft(mayHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> mayDifference)))
              }
              val mustHeapMap = mustObjR.foldLeft(mustHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> mustDifference)))
              }
              copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
            }
          case (left: VariableIdentifier, Constant("null", _, _)) => // e.g., x != null
            // set of heap nodes that may/must be pointed to by the left identifier
            val mayL = mayStore.getOrElse(left, Set.empty)
            val mustL = mustStore.getOrElse(left, Set.empty)
            val mayIntersection = mayL intersect Set[HeapNode](NullHeapNode)
            val mustIntersection = mustL intersect Set[HeapNode](NullHeapNode)
            val mayDifference = mayL diff mayIntersection
            val mustDifference = mustL diff mustIntersection
            if ((mayDifference.isEmpty && mayIntersection.size == 1) || (mustDifference.isEmpty && mustIntersection.size == 1))
              this.bottom() // there is no different heap node
            else { // there is at least one different heap node
              // return the current state with updated store
              val mayStoreMap = mayStore + (left -> mayDifference)
              val mustStoreMap = mustStore + (left -> mustDifference)
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap).pruneUnreachableHeap()
            }
          case (left: VariableIdentifier, right: VariableIdentifier) => // e.g, x != y
            // set of heap nodes that may/must be pointed to by the left identifier
            val mayL = mayStore.getOrElse(left, Set.empty)
            val mustL = mustStore.getOrElse(left, Set.empty)
            // set of heap nodes that may/must be pointed to by the right identifier
            val mayR = mayStore.getOrElse(right, Set.empty)
            val mustR = mustStore.getOrElse(right, Set.empty)
            val mayIntersection = mayL intersect mayR
            val mustIntersection = mustL intersect mustR
            val mayDifferenceL = mayL diff mayIntersection
            val mustDifferenceL = mustL diff mustIntersection
            val mayDifferenceR = mayR diff mayIntersection
            val mustDifferenceR = mustR diff mustIntersection
            if ((mayDifferenceL.isEmpty && mayDifferenceR.isEmpty && mayIntersection.size == 1) ||
              (mustDifferenceL.isEmpty && mustDifferenceR.isEmpty && mustIntersection.size == 1))
              this.bottom() // there is no different heap node
            else { // there is at least one different heap node
              // return the current state with updated store
              val mayStoreMap = mayStore + (left -> mayDifferenceL, right -> mayDifferenceR)
              val mustStoreMap = mustStore + (left -> mustDifferenceL, right -> mustDifferenceR)
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap).pruneUnreachableHeap()
            }
          case (left: VariableIdentifier, AccessPathIdentifier(right)) => // e.g., x != y.f
            // set heap nodes pointed to by the left identifier
            val mayL = mayStore.getOrElse(left, Set.empty)
            val mustL = mustStore.getOrElse(left, Set.empty)
            val mayObjR = mayEvaluateReceiver(right) // set of (may right) receivers
            val mustObjR = mustEvaluateReceiver(right) // set of (must right) receivers
            val mayR = mayObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the right path
            val mustR = mustObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the right path
            val mayIntersection = mayL intersect mayR
            val mustIntersection = mustL intersect mustR
            val mayDifferenceL = mayL diff mayIntersection
            val mustDifferenceL = mustL diff mustIntersection
            val mayDifferenceR = mayR diff mayIntersection
            val mustDifferenceR = mustR diff mustIntersection
            if ((mayDifferenceL.isEmpty && mayDifferenceR.isEmpty && mayIntersection.size == 1) ||
              (mustDifferenceL.isEmpty && mustDifferenceR.isEmpty && mustIntersection.size == 1))
              this.bottom() // there is no different heap node
            else { // there is at least one different heap node
              // return the current state with updated store and heap
              val mayStoreMap = mayStore + (left -> mayDifferenceL)
              val mustStoreMap = mustStore + (left -> mustDifferenceL)
              val mayHeapMap = mayObjR.foldLeft(mayHeap) {
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> mayDifferenceR)))
              }
              val mustHeapMap = mustObjR.foldLeft(mustHeap) {
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> mustDifferenceR)))
              }
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap, mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), Constant("null", _, _)) => // e.g., x.f == null
            val mayObjL = mayEvaluateReceiver(left) // set of (may left) receivers
            val mustObjL = mustEvaluateReceiver(left) // set of (must left) receivers
            val mayL = mayObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the left path
            val mustL = mustObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the left path
            val mayIntersection = mayL intersect Set[HeapNode](NullHeapNode)
            val mustIntersection = mustL intersect Set[HeapNode](NullHeapNode)
            val mayDifference = mayL diff mayIntersection
            val mustDifference = mustL diff mustIntersection
            if ((mayDifference.isEmpty && mayIntersection.size == 1) || (mustDifference.isEmpty && mustIntersection.size == 1))
              this.bottom() // there is no different heap node
            else { // there is at least one different heap node
              // return the current state with updated heap
              val mayHeapMap = mayObjL.foldLeft(mayHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> mayDifference)))
              }
              val mustHeapMap = mustObjL.foldLeft(mustHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> mustDifference)))
              }
              copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), right: VariableIdentifier) => // e.g., x.f != y
            val mayObjL = mayEvaluateReceiver(left) // set of (may left) receivers
            val mustObjL = mustEvaluateReceiver(left) // set of (must left) receivers
            val mayL = mayObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the left path
            val mustL = mustObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the left path
            // set of heap nodes that may/must be pointed to by the right identifier
            val mayR = mayStore.getOrElse(right, Set.empty)
            val mustR = mustStore.getOrElse(right, Set.empty)
            val mayIntersection = mayL intersect mayR
            val mustIntersection = mustL intersect mustR
            val mayDifferenceL = mayL diff mayIntersection
            val mustDifferenceL = mustL diff mustIntersection
            val mayDifferenceR = mayR diff mayIntersection
            val mustDifferenceR = mustR diff mustIntersection
            if ((mayDifferenceL.isEmpty && mayDifferenceR.isEmpty && mayIntersection.size == 1) ||
              (mustDifferenceL.isEmpty && mustDifferenceR.isEmpty && mustIntersection.size == 1))
              this.bottom() // there is no different heap node
            else { // there is at least one different heap node
              // return the current state with updated store and heap
              val mayHeapMap = mayObjL.foldLeft(mayHeap) {
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> mayDifferenceL)))
              }
              val mustHeapMap = mustObjL.foldLeft(mustHeap) {
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> mustDifferenceL)))
              }
              val mayStoreMap = mayStore + (right -> mayDifferenceR)
              val mustStoreMap = mustStore + (right -> mustDifferenceR)
              copy(mayStore = mayStoreMap, mustStore = mustStoreMap, mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), AccessPathIdentifier(right)) => // e.g., x.f != y.f
            val mayObjL = mayEvaluateReceiver(left) // set of (may left) receivers
            val mustObjL = mustEvaluateReceiver(left) // set of (must left) receivers
            val mayObjR = mayEvaluateReceiver(right) // set of (may right) receivers
            val mustObjR = mustEvaluateReceiver(right) // set of (must right) receivers
            val mayL = mayObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the left path
            val mustL = mustObjL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the left path
            val mayR = mayObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mayHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that may be pointed to by the right path
            val mustR = mustObjR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ mustHeap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes that must be pointed to by the right path
            val mayIntersection = mayL intersect mayR
            val mustIntersection = mustL intersect mustR
            val mayDifferenceL = mayL diff mayIntersection
            val mustDifferenceL = mustL diff mustIntersection
            val mayDifferenceR = mayR diff mayIntersection
            val mustDifferenceR = mustR diff mustIntersection
            if ((mayDifferenceL.isEmpty && mayDifferenceR.isEmpty && mayIntersection.size == 1) ||
              (mustDifferenceL.isEmpty && mustDifferenceR.isEmpty && mustIntersection.size == 1))
              this.bottom() // there is no different heap node
            else { // there is at least one different heap node
              // return the current state with updated heap
              var mayHeapMap = mayObjL.foldLeft(mayHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> mayDifferenceL)))
              }
              mayHeapMap = mayObjR.foldLeft(mayHeapMap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> mayDifferenceR)))
              }
              var mustHeapMap = mustObjL.foldLeft(mustHeap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> mustDifferenceL)))
              }
              mustHeapMap = mustObjR.foldLeft(mustHeapMap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> mustDifferenceR)))
              }
              copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
            }
        }
      case _ => throw new NotImplementedError("An assume implementation is missing.")
    }
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): T = {
    logger.trace("\n*** ----------------before(" + pp.toString + "): " + this.repr)
    copy(currentPP = pp) // return the current state with updated currentPP
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): T = copy(isBottom = true, isTop = false)

  def copy(fields: Set[(Type, String)] = fields,
           currentPP: ProgramPoint = currentPP,
           materialization: Boolean = materialization,
           result: ExpressionSet = result,
           mayStore: Map[VariableIdentifier,Set[HeapNode]] = mayStore,
           mustStore: Map[VariableIdentifier,Set[HeapNode]] = mustStore,
           mayHeap: Map[HeapNode,Map[String, Set[HeapNode]]] = mayHeap,
           mustHeap: Map[HeapNode,Map[String, Set[HeapNode]]] = mustHeap,
           isBottom: Boolean = isBottom,
           isTop: Boolean = isTop): T

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): T = {
    logger.trace("*** ----------------createObject(" + typ.toString + "; " + pp.toString + ")")

    val fresh = HeapNode(List.empty[Identifier],currentPP) // create fresh heap node
    // update the may heap map with the fresh node
    val mayFieldMap = fields.foldLeft(Map.empty[String,Set[HeapNode]]) {
      case (map, (_: RefType, field)) => map + (field -> Set(SummaryHeapNode, NullHeapNode))
      case (map, _) => map
    }
    val mayHeapMap = mayHeap + (fresh -> mayFieldMap)
    // update the must heap map with the fresh node
    val mustFieldMap = fields.foldLeft(Map.empty[String,Set[HeapNode]]) {
      case (map, (_: RefType, field)) => map + (field -> Set(UnknownHeapNode))
      case (map, _) => map
    }
    val mustHeapMap = mustHeap + (fresh -> mustFieldMap)
    // return the current state with updated result, store, heap
    copy(result = ExpressionSet(fresh), mayHeap = mayHeapMap, mustHeap = mustHeapMap)
  }

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    logger.trace("*** ----------------createVariable(" + x.toString + "; " + typ.toString + "; " + pp.toString + ")")

    if (typ.isObject) { // the variable to be created is a Ref
      val mayStoreMap = mayStore + (x -> Set[HeapNode](NullHeapNode))
      val mustStoreMap = mustStore + (x -> Set[HeapNode](NullHeapNode))
      copy(result = ExpressionSet(x), mayStore = mayStoreMap, mustStore = mustStoreMap)
    } else this // the variable to be created is not a Ref
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): T = {
    logger.trace("*** ----------------createVariableForArgument(" + x.toString + "; " + typ.toString + ")")

    if (typ.isObject) { // the variable to be created is a Ref
      // add key to store map
      val mayStoreMap = mayStore + (x -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
      val mustStoreMap = mustStore + (x -> Set[HeapNode](UnknownHeapNode))
      // return the current state with updated result and store
      copy(result = ExpressionSet(x), mayStore = mayStoreMap, mustStore = mustStoreMap)
    } else this // the variable to be created is not a Ref
  }

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    // return the current state with updated result
    copy(result = ExpressionSet(new Constant(value, typ, pp)))
  }

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  private def exhale(acc: Expression): T = {
    logger.trace("*** exhale(" + acc.toString + ")")

    acc match {
      case acc: PermissionExpression => {
        acc.id match {
          case AccessPathIdentifier(path) =>
            val mayObj = mayEvaluateReceiver(path) // set of (path) receivers
            val mustObj = mustEvaluateReceiver(path)
            // havoc the heap pointed to by the path
            var mayHeapMap = if (!mayHeap.contains(SummaryHeapNode)) {
              var fieldMap = Map.empty[String,Set[HeapNode]]
              for (f <- fields) { // for all fields declared within the program...
                f._1 match {
                  case _:RefType => fieldMap = fieldMap + (f._2 -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
                }
              }
              mayHeap + (SummaryHeapNode -> fieldMap) // add summary node to heap map
            } else { mayHeap }
            mayHeapMap = mayObj.foldLeft(mayHeapMap) {
              case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (path.last.getName -> mayHeapMap.keySet)))
            }
            val mustHeapMap = mustObj.foldLeft(mustHeap) {
              case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (path.last.getName -> Set.empty)))
            }
            copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap).pruneUnreachableHeap()
          case _ => throw new IllegalArgumentException("A permission exhale must occur via an Access Path Identifier")
        }
      }
      case _ =>
        // we do not assert boolean conditions since the analysis would fail
        // in all cases where we are not able to prove that something holds.
        this
    }
  }

  /** Returns the current result. */
  override def expr: ExpressionSet = this.result

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): T = ???

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object on which the field access is performed
    * @param field the name of the field
    * @param typ   the type of the field
    * @return The abstract state obtained after the field access, that is,
    *         a new state whose `ExpressionSet` holds the symbolic representation of the value of the given field.
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): T = {
    logger.trace("*** ----------------getFieldValue(" + obj.toString + "; " + field + "; " + typ.toString + ")")

    val last = VariableIdentifier(field)(typ)
    obj match {
      case head: VariableIdentifier =>
        val access = List(head, last) // full access path
        val state = materialize(access) // materialization
        // return the current state with updated result, store, heap
        val res = AccessPathIdentifier(access)
        copy(result = ExpressionSet(res), mayStore = state.mayStore, mayHeap = state.mayHeap,
          mustStore = state.mustStore, mustHeap = state.mustHeap)
      case AccessPathIdentifier(path) =>
        val access = path :+ last // full access path
        val state = materialize(access) // materialization
        // return the current state with updated result, store, heap
        val res = AccessPathIdentifier(access)
        copy(result = ExpressionSet(res), mayStore = state.mayStore, mayHeap = state.mayHeap,
          mustStore = state.mustStore, mustHeap = state.mustHeap)
      case _ => throw new IllegalArgumentException("A field access must occur via an Identifier")
    }
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): T = {
    logger.trace("*** ----------------getVariableValue(" + id.toString + ")")

    val x = id.asInstanceOf[VariableIdentifier]
    val mayRcvSet = mayStore.getOrElse(x,Set.empty) // set of may (path) receivers
    val mustRcvSet = mustStore.getOrElse(x,Set.empty) // set of must (path) receivers
    var mayStoreMap = mayStore
    var mustStoreMap = mustStore
    var mayHeapMap = mayHeap
    var mustHeapMap = mustHeap
    if (mayRcvSet.contains(SummaryHeapNode) && materialization) { // materialization
      val fresh = HeapNode(List[Identifier](id)) // create fresh heap node
      // add key to store map to replace the summary node with the fresh node
      mayStoreMap = mayStoreMap + (x -> (mayRcvSet - SummaryHeapNode + fresh))
      mayHeapMap = mayHeapMap + (fresh -> mayHeapMap(SummaryHeapNode)) // update heap map with the fresh node
    }
    if (mustRcvSet.contains(UnknownHeapNode) && materialization) { // materialization
      val fresh = HeapNode(List[Identifier](id)) // create fresh heap node
      // add key to store map to replace the unknown node with the fresh node
      mustStoreMap = mustStoreMap + (x -> (mustRcvSet - UnknownHeapNode + fresh))
      mustHeapMap = mustHeapMap + (fresh -> mustHeapMap(UnknownHeapNode)) // update heap map with the fresh node
    }
    // return the current state with updated result, store, heap
    copy(result = ExpressionSet(id), mayStore = mayStoreMap, mustStore = mustStoreMap, mayHeap = mayHeapMap, mustHeap = mustHeapMap)
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: T): T = {
    logger.trace("*** glb(" + this.repr + ", " + other.repr + ")")

    def mayZipper[K](map1: Map[K,Set[HeapNode]], map2: Map[K,Set[HeapNode]]): Map[K,Set[HeapNode]] = {
      var keyMap = Map[K,Set[HeapNode]]()
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,_) => // nothing to be done
          case (_,None) => // nothing to be done
          case (Some(o1),Some(o2)) => keyMap = keyMap + (key -> (o1 & o2))
        }
      }; keyMap
    }
    def mustZipper[K](map1: Map[K,Set[HeapNode]], map2: Map[K,Set[HeapNode]]): Map[K,Set[HeapNode]] = {
      var keyMap = Map.empty[K,Set[HeapNode]]
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,None) => // nothing to be done
          case (None,Some(o2)) => keyMap = keyMap + (key -> o2)
          case (Some(o1),None) => keyMap = keyMap + (key -> o1)
          case (Some(o1),Some(o2)) => // we keep the summary node only if present in both maps
            val o = (o1 - UnknownHeapNode) ++ (o2 - UnknownHeapNode) ++ (o1 & o2)
            keyMap = keyMap + (key -> o)
        }
      }; keyMap
    }
    val fieldSet = this.fields & other.fields  // meet the fieldSets
    val allowed = this.materialization && other.materialization // meet the materialization flags
    val expr = this.result glb other.result // meet the exprSets
    // merge the stores
    val mayStoreMap = mayZipper[VariableIdentifier](this.mayStore,other.mayStore)
    val mustStoreMap = mustZipper[VariableIdentifier](this.mustStore,other.mustStore)
    // merge the heaps
    var mayHeapMap = Map.empty[HeapNode,Map[String,Set[HeapNode]]]
    for (key <- this.mayHeap.keySet ++ other.mayHeap.keySet) { // for all keys present in either map...
      (this.mayHeap.get(key),other.mayHeap.get(key)) match {
        case (None,None) =>
        case (None,Some(m2)) => mayHeapMap = mayHeapMap + (key -> m2)
        case (Some(m1),None) => mayHeapMap = mayHeapMap + (key -> m1)
        case (Some(m1: Map[String,Set[HeapNode]]),Some(m2: Map[String,Set[HeapNode]])) =>
          mayHeapMap = mayHeapMap + (key -> mayZipper[String](m1,m2))
      }
    }
    var mustHeapMap = Map.empty[HeapNode,Map[String,Set[HeapNode]]]
    for (key <- this.mustHeap.keySet ++ other.mustHeap.keySet) { // for all keys present in either map...
      (this.mustHeap.get(key),other.mustHeap.get(key)) match {
        case (None,_) => // nothing to be done
        case (_,None) => // nothing to be done
        case (Some(m1: Map[String,Set[HeapNode]]),Some(m2: Map[String,Set[HeapNode]])) =>
          mustHeapMap = mustHeapMap + (key -> mustZipper[String](m1,m2))
      }
    }
    // return the current state with updated result, store, heap
    copy(fields = fieldSet, currentPP = DummyProgramPoint, materialization = allowed, result = expr,
      mayStore = mayStoreMap, mustStore = mustStoreMap, mayHeap = mayHeapMap, mustHeap = mustHeapMap,
      isBottom = this.isBottom || other.isBottom, isTop = this.isTop && other.isTop)
  }


  override def command(cmd: Command): T = cmd match {
    case InhaleCommand(expression) => unlessBottom(expression, Lattice.bigLub(expression.getNonTop.map(inhale)))
    case ExhaleCommand(expression) => unlessBottom(expression, Lattice.bigLub(expression.getNonTop.map(exhale)))
    case PreconditionCommand(condition) => command(InhaleCommand(condition))
    case PostconditionCommand(condition) => command(ExhaleCommand(condition))
    case InvariantCommand(condition) => command(ExhaleCommand(condition)).command(InhaleCommand(condition))
    case _ => super.command(cmd)
  }

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  private def inhale(acc: Expression): T = {
    logger.trace("*** inahle(" + acc.toString + ")")

    acc match {
      case acc: PermissionExpression => this // nothing to be done
      case _ => this.assume(acc) // assume
    }
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: T): Boolean = {
    logger.trace("*** lessEqual(" + this.repr + ", " + other.repr + ")")

    if (this.isBottom || other.isTop) return true
    else if (other.isBottom || this.isTop) return false

    val mayStoreMap = this.mayStore.forall {
      case (k: VariableIdentifier, s: Set[HeapNode]) => {
        val oo = other.mayStore.getOrElse(k, Set.empty)
        (!oo.contains(SummaryHeapNode) || s.contains(SummaryHeapNode)) &&
          ((s - SummaryHeapNode) subsetOf (oo - SummaryHeapNode))
      }
    } // compare the (may) stores
    val mustStoreMap = this.mustStore.forall {
      case (k: VariableIdentifier, s: Set[HeapNode]) => {
        val oo = other.mustStore.getOrElse(k, Set.empty)
        (!oo.contains(UnknownHeapNode) || s.contains(UnknownHeapNode)) &&
          ((oo - UnknownHeapNode) subsetOf (s - UnknownHeapNode))
      }
    } // compare the (must) stores
    val mayHeapMap = this.mayHeap.forall {
      case (o: HeapNode, m: Map[String, Set[HeapNode]]) => m.forall {
        case (f: String, s: Set[HeapNode]) => {
          val oo = other.mayHeap.getOrElse(o, Map.empty).getOrElse(f, Set.empty)
          (!oo.contains(SummaryHeapNode) || s.contains(SummaryHeapNode)) &&
            ((s - SummaryHeapNode) subsetOf (oo - SummaryHeapNode))
        }
      }
    } // compare the (may) heaps
    val mustHeapMap = this.mustHeap.forall {
      case (o: HeapNode, m: Map[String, Set[HeapNode]]) => m.forall {
        case (f: String, s: Set[HeapNode]) => {
          val oo = other.mustHeap.getOrElse(o, Map.empty).getOrElse(f, Set.empty)
          (!oo.contains(UnknownHeapNode) || s.contains(UnknownHeapNode)) &&
            ((oo - UnknownHeapNode) subsetOf (s - UnknownHeapNode))
        }
      }
    } // compare the (must) heaps
    mayStoreMap && mayHeapMap && mustStoreMap && mustHeapMap
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: T): T = {
    logger.trace("*** lub(" + this.repr + ", " + other.repr + ")")

    def mayZipper[K](map1: Map[K,Set[HeapNode]], map2: Map[K,Set[HeapNode]]): Map[K,Set[HeapNode]] = {
      var keyMap = Map.empty[K,Set[HeapNode]]
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,None) =>
          case (None,Some(o2)) => keyMap = keyMap + (key -> o2)
          case (Some(o1),None) => keyMap = keyMap + (key -> o1)
          case (Some(o1),Some(o2)) => // we keep the summary node only if present in both maps
            val o = (o1 - SummaryHeapNode) ++ (o2 - SummaryHeapNode) ++ (o1 & o2)
            keyMap = keyMap + (key -> o)
        }
      }; keyMap
    }
    def mustZipper[K](map1: Map[K,Set[HeapNode]], map2: Map[K,Set[HeapNode]]): Map[K,Set[HeapNode]] = {
      var keyMap = Map.empty[K,Set[HeapNode]]
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,_) => // nothing to be done
          case (_,None) => // nothing to be done
          case (Some(o1),Some(o2)) => keyMap = keyMap + (key -> (o1 & o2))
        }
      }; keyMap
    }
    val fieldSet = this.fields ++ other.fields  // join the fieldSets
    val allowed = this.materialization || other.materialization // join the materialization flags
    val expr = this.result lub other.result // join the exprSets
    // merge the stores
    val mayStoreMap = mayZipper[VariableIdentifier](this.mayStore,other.mayStore)
    val mustStoreMap = mustZipper[VariableIdentifier](this.mustStore,other.mustStore)
    // merge the heaps
    var mayHeapMap = Map.empty[HeapNode,Map[String,Set[HeapNode]]]
    for (key <- this.mayHeap.keySet ++ other.mayHeap.keySet) { // for all keys present in either map...
      (this.mayHeap.get(key),other.mayHeap.get(key)) match {
        case (None,None) =>
        case (None,Some(m2)) => mayHeapMap = mayHeapMap + (key -> m2)
        case (Some(m1),None) => mayHeapMap = mayHeapMap + (key -> m1)
        case (Some(m1: Map[String,Set[HeapNode]]),Some(m2: Map[String,Set[HeapNode]])) =>
          mayHeapMap = mayHeapMap + (key -> mayZipper[String](m1,m2))
      }
    }
    var mustHeapMap = Map.empty[HeapNode,Map[String,Set[HeapNode]]]
    for (key <- this.mustHeap.keySet ++ other.mustHeap.keySet) { // for all keys present in either map...
      (this.mustHeap.get(key),other.mustHeap.get(key)) match {
        case (None,_) => // nothing to be done
        case (_,None) => // nothing to be done
        case (Some(m1: Map[String,Set[HeapNode]]),Some(m2: Map[String,Set[HeapNode]])) =>
          mustHeapMap = mustHeapMap + (key -> mustZipper[String](m1,m2))
      }
    }
    // return the current state with updated result, store, heap
    copy(fields = fieldSet, currentPP = DummyProgramPoint, materialization = allowed, result = expr,
      mayStore = mayStoreMap, mustStore = mustStoreMap, mayHeap = mayHeapMap, mustHeap = mustHeapMap,
      isBottom = this.isBottom && other.isBottom, isTop = this.isTop || other.isTop)
  }

  def materialize(path: AccessPath): T =
  {
    logger.trace("*** ----------------materialize(" + path.toString + ")")

    var mayStoreMap: Map[VariableIdentifier, Set[HeapNode]] = mayStore // new store map (initially equal to store)
    var mustStoreMap: Map[VariableIdentifier, Set[HeapNode]] = mustStore
    var mayHeapMap: Map[HeapNode, Map[String, Set[HeapNode]]] = mayHeap // new heap map (initially equal to heap)
    var mustHeapMap: Map[HeapNode, Map[String, Set[HeapNode]]] = mustHeap
    // path head evaluation
    val head = path.head.asInstanceOf[VariableIdentifier]
    var mayRcvSet: Set[HeapNode] = mayStoreMap.getOrElse(head, Set.empty)  // initial set of (path) receivers
    var mustRcvSet: Set[HeapNode] = mustStoreMap.getOrElse(head, Set.empty)
    if (mayRcvSet.contains(SummaryHeapNode) && materialization) { // materialization
    val fresh = HeapNode(List[Identifier](head)) // create fresh heap node
      mayRcvSet = mayRcvSet - SummaryHeapNode + fresh // update receiver set
      mayStoreMap = mayStoreMap + (head -> mayRcvSet) // add key to store map to replace the summary node with the fresh node
      mayHeapMap = mayHeapMap + (fresh -> mayHeapMap(SummaryHeapNode)) // update heap map with the fresh node
    }
    if (mustRcvSet.contains(UnknownHeapNode) && materialization) { // materialization
      val fresh = HeapNode(List[Identifier](head)) // create fresh heap node
      mustRcvSet = mustRcvSet - UnknownHeapNode + fresh // update receiver set
      mustStoreMap = mustStoreMap + (head -> mustRcvSet) // add key to store map to replace the summary node with the fresh node
      mustHeapMap = mustHeapMap + (fresh -> mustHeapMap(UnknownHeapNode)) // update heap map with the fresh node
    }
    // path tail evaluation
    val mayEval: Set[HeapNode] = path.drop(1).dropRight(1).foldLeft(mayRcvSet)(
      (rcv: Set[HeapNode],id: Identifier) => {  // for all following path segments...
        if (rcv.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP)
        rcv.foldLeft(Set.empty[HeapNode])(
          (set: Set[HeapNode],node: HeapNode) => {  // for all current receivers...
          var curr: Set[HeapNode] = mayHeapMap.getOrElse(node,Map.empty).getOrElse(id.getName,Set.empty)
            if (curr.contains(SummaryHeapNode) && materialization) { // materialization
            val fresh = HeapNode(node.id :+ id) // create fresh heap node
              curr = curr - SummaryHeapNode + fresh // update the current receiver set
              // add key to heap map to replace the summary node with the fresh node
              mayHeapMap = mayHeapMap + (node -> (mayHeapMap.getOrElse(node,Map.empty) + (id.getName -> curr)))
              mayHeapMap = mayHeapMap + (fresh -> mayHeapMap(SummaryHeapNode)) // update heap map with the fresh node
            }
            set ++ curr
        })
    })
    val mustEval: Set[HeapNode] = path.drop(1).dropRight(1).foldLeft(mustRcvSet)(
      (rcv: Set[HeapNode],id: Identifier) => {  // for all following path segments...
        if (rcv.contains(NullHeapNode)) Reporter.reportError("Null pointer dereference", currentPP)
        rcv.foldLeft(Set.empty[HeapNode])(
          (set: Set[HeapNode],node: HeapNode) => {  // for all current receivers...
          var curr: Set[HeapNode] = mustHeapMap.getOrElse(node,Map.empty).getOrElse(id.getName,Set.empty)
            if (curr.contains(UnknownHeapNode) && materialization) { // materialization
              val fresh = HeapNode(node.id :+ id) // create fresh heap node
              curr = curr - UnknownHeapNode + fresh // update the current receiver set
              // add key to heap map to replace the summary node with the fresh node
              mustHeapMap = mustHeapMap + (node -> (mustHeapMap.getOrElse(node,Map.empty) + (id.getName -> curr)))
              mustHeapMap = mustHeapMap + (fresh -> mustHeapMap(UnknownHeapNode)) // update heap map with the fresh node
            }
            set ++ curr
        })
    })
    if (mayEval.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP)
    if (mustEval.contains(NullHeapNode)) Reporter.reportError("Null pointer dereference", currentPP)
    // path end evaluation
    val last = path.last.asInstanceOf[VariableIdentifier]
    if (last.typ.isObject) { // the accessed field is a Ref
    val mayLast = mayEval.foldLeft(Set.empty[HeapNode])(
        (set: Set[HeapNode],node: HeapNode) => {  // for all remaining receivers...
        var curr: Set[HeapNode] = mayHeapMap.getOrElse(node,Map.empty).getOrElse(last.getName,Set.empty)
          if (curr.contains(SummaryHeapNode) && materialization) { // materialization
          val fresh = HeapNode(node.id :+ last) // create fresh heap node
            curr = curr - SummaryHeapNode + fresh // update the current receiver set
            // add key to heap map to replace the summary node with the fresh node
            mayHeapMap = mayHeapMap + (node -> (mayHeapMap.getOrElse(node,Map.empty) + (last.getName -> curr)))
            mayHeapMap = mayHeapMap + (fresh -> mayHeapMap(SummaryHeapNode)) // update heap map with the fresh node
          }
          set ++ curr
        }
      )
      val mustLast = mustEval.foldLeft(Set.empty[HeapNode])(
        (set: Set[HeapNode],node: HeapNode) => {  // for all remaining receivers...
        var curr: Set[HeapNode] = mustHeapMap.getOrElse(node,Map.empty).getOrElse(last.getName,Set.empty)
          if (curr.contains(UnknownHeapNode) && materialization) { // materialization
            val fresh = HeapNode(node.id :+ last) // create fresh heap node
            curr = curr - UnknownHeapNode + fresh // update the current receiver set
            // add key to heap map to replace the summary node with the fresh node
            mustHeapMap = mustHeapMap + (node -> (mustHeapMap.getOrElse(node,Map.empty) + (last.getName -> curr)))
            mustHeapMap = mustHeapMap + (fresh -> mustHeapMap(UnknownHeapNode)) // update heap map with the fresh node
          }
          set ++ curr
        }
      )
      if (mayLast.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP)
      if (mustLast.contains(NullHeapNode)) Reporter.reportError("Null pointer dereference", currentPP)
    }
    copy(mayStore = mayStoreMap, mayHeap = mayHeapMap, mustStore = mustStoreMap, mustHeap = mustHeapMap)
  }

  /**
    * Evaluates an access path with respect to the may alias analysis.
    *
    * @param path the access path to evaluate
    */
  def mayEvaluatePath(path: AccessPath): Set[HeapNode] = {
    val first = mayStore.getOrElse(path.head.asInstanceOf[VariableIdentifier],Set.empty) // path head evaluation
    val eval = path.drop(1).foldLeft(first)( // path tail evaluation
        (set,next) => { // next path segment
          if (set.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP)
          set.foldLeft(Set.empty[HeapNode])(
            (s,obj) => s ++ mayHeap.getOrElse(obj,Map.empty).getOrElse(next.getName,Set.empty)
          )}
      ) // return the objects referenced by the path (except the last field)
    if (eval.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP); eval
  }

  /**
    * Evaluates an access path with respect to the may alias analysis up to but
    * not including the last field.
    *
    * @param path the access path to evaluate
    */
  def mayEvaluateReceiver(path: AccessPath) : Set[HeapNode] =
    mayEvaluatePath(path.dropRight(1))

  /**
    * Evaluates an access path with respect to the must alias analysis.
    *
    * @param path the access path to evaluate
    */
  def mustEvaluatePath(path: AccessPath): Set[HeapNode] = {
    val first = mustStore.getOrElse(path.head.asInstanceOf[VariableIdentifier],Set.empty) // path head evaluation
    val eval = path.drop(1).foldLeft(first)( // path tail evaluation
        (set,next) => { // next path segment
          if (set.contains(NullHeapNode)) Reporter.reportError("Null pointer dereference", currentPP)
          set.foldLeft(Set.empty[HeapNode])(
            (s,obj) => s ++ mustHeap.getOrElse(obj,Map.empty).getOrElse(next.getName,Set.empty)
          )}
      ) // return the objects referenced by the path (except the last field)
    if (eval.contains(NullHeapNode)) Reporter.reportError("Null pointer dereference", currentPP); eval
  }

  /**
    * Evaluates an access path with respect to the must alias analysis up to but
    * not including the last field
    *
    * @param path the access path to evaluate
    */
  def mustEvaluateReceiver(path: AccessPath) : Set[HeapNode] =
    mustEvaluatePath(path.dropRight(1))

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): T = {
    // retrieve all heap nodes reachable from variables
    var mayReach = mayStore.foldLeft(Set[HeapNode]())((r, s) => r ++ s._2)
    var mustReach = mustStore.foldLeft(Set[HeapNode]())((r, s) => r ++ s._2)
    // add all heap nodes reachable from fields of heap nodes reachable from variables
    mayReach = mayReach ++ mayReach.foldLeft(Set.empty[HeapNode])(
      (set, node) => set ++ mayHeap.getOrElse(node,Map.empty).foldLeft(Set.empty[HeapNode])((r, s) => r ++ s._2)
    )
    mustReach = mustReach ++ mustReach.foldLeft(Set.empty[HeapNode])(
      (set, node) => set ++ mustHeap.getOrElse(node,Map.empty).foldLeft(Set.empty[HeapNode])((r, s) => r ++ s._2)
    )
    // remove all unreachable heap nodes
    var mayHeapMap = mayHeap; for (key <- mayHeap.keySet diff mayReach) { mayHeapMap = mayHeapMap - key }
    var mustHeapMap = mustHeap; for (key <- mustHeap.keySet diff mustReach) { mustHeapMap = mustHeapMap - key }
    // return the current state with updated heap
    copy(mayHeap = mayHeapMap, mustHeap = mustHeapMap)
  }

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = ???

  /**
    * Returns whether the specified access paths may alias.
    *
    * @param first the first access path
    * @param second the second access path
    */
  def pathsMayAlias(first: AccessPath, second: AccessPath): Boolean = {
    val evalFirst = mayEvaluatePath(first)
    val evalSecond = mayEvaluatePath(second)
    val intersection = evalFirst intersect evalSecond
    (intersection - NullHeapNode).nonEmpty
  }

  /**
    * Returns whether the specified access paths must alias.
    *
    * @param first  the first access path
    * @param second the second access path
    */
  def pathsMustAlias(first: AccessPath, second: AccessPath): Boolean =  {
    val evalFirst = mustEvaluatePath(first) -- Set(NullHeapNode, UnknownHeapNode)
    val evalSecond = mustEvaluatePath(second) -- Set(NullHeapNode, UnknownHeapNode)
    evalFirst.size == 1 && evalSecond.size == 1 && evalFirst == evalSecond
  }

  /**
    * Returns whether the receivers of the given access paths may alias.
    *
    * @param first the first access path
    * @param second the second access path
    */
  def receiversMayAlias(first: AccessPath, second: AccessPath): Boolean =
    pathsMayAlias(first.dropRight(1), second.dropRight(1))

  /**
    * Returns whether the receivers of the specified access paths must alias.
    *
    * @param first  the first access path
    * @param second the second access path
    */
  def receiversMustAlias(first: AccessPath, second: AccessPath): Boolean =
    pathsMustAlias(first.dropRight(1), second.dropRight(1))

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression
    */
  override def removeExpression(): T = copy(result = ExpressionSet())

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    */
  override def removeVariable(varExpr: VariableIdentifier): T = ???

  /** The default state string representation.
    *
    * @return the default string representation of the current state
    */
  def repr: String = s"AliasAnalysisState( $result, $mayStore, $mustStore, $mayHeap, $mustHeap )"

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): T = ???

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): T = copy(result = expr)

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  override def setVariableToTop(varExpr: Expression): T = ???

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    */
  override def throws(t: ExpressionSet): T = ???

  /** The state string representation.
    *
    * @return the string representation of the current state
    */
  override def toString: String = s"AliasAnalysisState(\n\tresult: $result\n\t" +
    s"mayStore: $mayStore\n\tmustStore: $mustStore\n\tmayHeap: $mayHeap\nmustHeap: $mustHeap\n)"

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): T = copy(isBottom = false, isTop = true)

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: T): T = {
    logger.trace("*** ----------------widening(" + other.repr + ")")

    def mayZipper[K](map1: Map[K,Set[HeapNode]], map2: Map[K,Set[HeapNode]]): Map[K,Set[HeapNode]] = {
      var keyMap = Map.empty[K,Set[HeapNode]]
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,None) =>
          case (None,Some(o2)) => keyMap = keyMap + (key -> o2)
          case (Some(o1),None) => keyMap = keyMap + (key -> o1)
          case (Some(o1),Some(o2)) => // we keep the summary node only if present in both maps
            val o = (o1 - SummaryHeapNode) ++ (o2 - SummaryHeapNode) ++ (o1 & o2)
            keyMap = keyMap + (key -> o)
        }
      }; keyMap
    }
    def mustZipper[K](map1: Map[K,Set[HeapNode]], map2: Map[K,Set[HeapNode]]): Map[K,Set[HeapNode]] = {
      var keyMap = Map.empty[K,Set[HeapNode]]
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,_) => // nothing to be done
          case (_,None) => // nothing to be done
          case (Some(o1),Some(o2)) => keyMap = keyMap + (key -> (o1 & o2))
        }
      }; keyMap
    }
    val fieldSet = this.fields ++ other.fields  // join the fieldSets
    val expr = this.result widening other.result // widen the exprSets
    // merge the stores
    val mayStoreMap = mayZipper[VariableIdentifier](this.mayStore,other.mayStore)
    val mustStoreMap = mustZipper[VariableIdentifier](this.mustStore,other.mustStore)
    // merge the heaps
    var mayHeapMap = Map.empty[HeapNode,Map[String,Set[HeapNode]]]
    for (key <- this.mayHeap.keySet ++ other.mayHeap.keySet) { // for all keys present in either map...
      (this.mayHeap.get(key),other.mayHeap.get(key)) match {
        case (None,None) =>
        case (None,Some(m2)) => mayHeapMap = mayHeapMap + (key -> m2)
        case (Some(m1),None) => mayHeapMap = mayHeapMap + (key -> m1)
        case (Some(m1: Map[String,Set[HeapNode]]),Some(m2: Map[String,Set[HeapNode]])) =>
          mayHeapMap = mayHeapMap + (key -> mayZipper[String](m1,m2))
      }
    }
    var mustHeapMap = Map.empty[HeapNode,Map[String,Set[HeapNode]]]
    for (key <- this.mayHeap.keySet ++ other.mayHeap.keySet) { // for all keys present in either map...
      (this.mayHeap.get(key),other.mayHeap.get(key)) match {
        case (None,_) => // nothing to be done
        case (_,None) => // nothing to be done
        case (Some(m1: Map[String,Set[HeapNode]]),Some(m2: Map[String,Set[HeapNode]])) =>
          mustHeapMap = mustHeapMap + (key -> mustZipper[String](m1,m2))
      }
    }
    // return the current state with updated result, store, heap
    copy(fields = fieldSet, currentPP = DummyProgramPoint, materialization = false, result = expr,
      mayStore = mayStoreMap, mustStore = mustStoreMap, mayHeap = mayHeapMap, mustHeap = mustHeapMap,
      isBottom = this.isBottom && other.isBottom, isTop = this.isTop || other.isTop)
  }
}

object AliasAnalysisState {
  case class Default(fields: Set[(Type, String)] = Set.empty,
                     currentPP: ProgramPoint = DummyProgramPoint,
                     materialization: Boolean = true,
                     result: ExpressionSet = ExpressionSet(),
                     mayStore: Map[VariableIdentifier, Set[HeapNode]] = Map.empty,
                     mustStore: Map[VariableIdentifier, Set[HeapNode]] = Map.empty,
                     mayHeap: Map[HeapNode, Map[String, Set[HeapNode]]] = Map.empty,
                     mustHeap: Map[HeapNode, Map[String, Set[HeapNode]]] = Map.empty,
                     isBottom: Boolean = false,
                     isTop: Boolean = false)
    extends AliasAnalysisState[Default] {

    override def copy(fields: Set[(Type, String)],
                      currentPP: ProgramPoint,
                      materialization: Boolean,
                      result: ExpressionSet,
                      mayStore: Map[VariableIdentifier, Set[HeapNode]],
                      mustStore: Map[VariableIdentifier, Set[HeapNode]],
                      mayHeap: Map[HeapNode, Map[String, Set[HeapNode]]],
                      mustHeap: Map[HeapNode, Map[String, Set[HeapNode]]],
                      isBottom: Boolean,
                      isTop: Boolean): Default =
      Default(fields, currentPP, materialization, result, mayStore, mustStore, mayHeap, mustHeap, isBottom, isTop)
  }
}

/** Alias Analysis Entry State Builder.
  *
  * @author Caterina Urban
  */
trait AliasAnalysisEntryStateBuilder[T <: AliasAnalysisState[T]] extends ForwardEntryStateBuilder[T] {

  protected var fields: Set[(Type,String)] = Set[(Type,String)]()

  override def build(method: MethodDeclaration): T = {
    // retrieve the set of fields declared within the program
    fields = Set[(Type,String)]()
    for(f <- method.classDef.fields) {
      fields = fields + ((f.typ, f.variable.toString))
    }
    // prepare the initial may heap map
    val mayFieldMap = fields.foldLeft(Map.empty[String,Set[HeapNode]]) {
      case (map, (_: RefType, field)) => map + (field -> Set(SummaryHeapNode, NullHeapNode))
      case (map, _) => map
    }
    val mayHeapMap = Map[HeapNode, Map[String, Set[HeapNode]]](SummaryHeapNode -> mayFieldMap)
    // prepare the initial must heap map
    val mustFieldMap = fields.foldLeft(Map.empty[String,Set[HeapNode]]) {
      case (map, (_: RefType, field)) => map + (field -> Set(UnknownHeapNode))
      case (map, _) => map
    }
    val mustHeapMap = Map[HeapNode, Map[String, Set[HeapNode]]](UnknownHeapNode -> mustFieldMap)
    // initialize the entry state
    method.initializeArgument[T](topState.copy(fields = fields, mayHeap = mayHeapMap, mustHeap = mustHeapMap))
  }

}

/** Alias Analysis Entry State.
  *
  * @author Jerome Dohrau, Caterina Urban
  */
object AliasAnalysisEntryState extends AliasAnalysisEntryStateBuilder[AliasAnalysisState.Default] {
  override def topState: AliasAnalysisState.Default = AliasAnalysisState.Default()
}

/** Alias Analysis Runner.
  *
  * @tparam S the backward permission state
  * @author Jerome Dohrau, Caterina Urban
  */
trait AliasAnalysisRunner[S <: AliasAnalysisState[S]] extends SilverAnalysisRunner[S] {

  override def main(args: Array[String]) {
    val results = run(new File(args(0)).toPath)

    println("\n*******************\n* Analysis Result *\n*******************\n")
    // map of method names to control flow graphs
    val methodNameToCfgState = results.map(result => result.method.name.toString -> result.cfgState).toMap
    for ((m, g) <- methodNameToCfgState) {
      println("******************* " + m + "\n")

      println(g.entryState()) // printing the entry state of the control-flow graph

      val blocks: List[List[Statement]] = g.cfg.nodes // blocks withing the control-flow graph
      // withing each block...
      var i = 0
      for (stmts: List[Statement] <- blocks) {
        if (stmts.isEmpty) {
          val states: List[S] = g.blockStates(i).last // post-states of each statement
          for (s <- states) {
            println("\n******************* \n")
            println(s)
          }
        } else {
          // printing the block pre-state
          println("\n+++++++++++++++++++ BLOCK " + i + "+++++++++++++++++++\n")
          println(g.blockStates(i).last.head)
          val states: List[S] = g.blockStates(i).last.drop(1) // post-states of each statement
          // print statements and corresponding post-states
          for ((c: Statement, s) <- stmts zip states) {
            println("\n******************* " + c + "\n")
            println(s)
          }
        }
        i = i + 1
      }

      println("\n******************* \n")
      println(g.exitState()) // printing the exit state of the control-flow graph
    }
  }

  override def toString = "Alias Analysis"
}

/** Alias Analysis.
  *
  * @author Jerome Dohrau, Caterina Urban
  */
object AliasAnalysis extends AliasAnalysisRunner[AliasAnalysisState.Default] {
  override val analysis = SimpleForwardAnalysis[AliasAnalysisState.Default](AliasAnalysisEntryState)
  override def toString = "Alias Analysis"
}
