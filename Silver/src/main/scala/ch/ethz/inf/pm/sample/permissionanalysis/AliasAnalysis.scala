package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{RefType, SilverAnalysisRunner}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.permissionanalysis.AttemptState.Default
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging

/** Heap node.
  *
  * @param id the unique identifier of the heap node
  * @author Caterina Urban
  */
case class HeapNode(id: List[Identifier] = List.empty, pp: ProgramPoint = DummyProgramPoint) extends Identifier
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

/**
  * @author Jerome Dohrau, Caterina Urban
  */
trait AliasAnalysisState[T <: AliasAnalysisState[T]]
  extends SimplePermissionState[T]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {
  this: T =>

  // set of fields declared in the program
  def fields: Set[(Type, String)]

  // current program point
  def currentPP: ProgramPoint

  // true = materialization allowed; false = materialization not allowed
  def materialization: Boolean

  // result of the previous statement
  def result: ExpressionSet

  // map from Ref variables to heap objects
  def store: Map[VariableIdentifier,Set[HeapNode]]

  // map from heap objects to a map from Ref fields to heap objects
  def heap: Map[HeapNode,Map[String, Set[HeapNode]]]

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
              val objR = evaluatePath(right) // set of (right) receivers
              val r = objR.foldLeft(Set.empty[HeapNode])(
                (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
              ) // set of heap nodes pointed to by the right path
              val rcvSet = evaluatePath(path) - NullHeapNode // set of path receivers (excluding null)
              val heapMap = rcvSet.foldLeft(heap)((map, node) => {
                if (node.representsSingleVariable) { // strong update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field -> r)))
                } else { // weak update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field ->
                    (map.getOrElse(node,Map.empty).getOrElse(field,Set.empty) ++ r))))
                }
              }) // update the heap
              // return the current state with updated heap
              copy(heap = heapMap).pruneUnreachableHeap()

            case right: Constant => // e.g., `x.f := null`
              val rcvSet = evaluatePath(path) - NullHeapNode // set of path receivers (excluding null)
              val heapMap = rcvSet.foldLeft(heap)((map, node) => {
                if (node.representsSingleVariable) { // strong update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field -> Set[HeapNode](NullHeapNode))))
                } else { // weak update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field ->
                    (map.getOrElse(node,Map.empty).getOrElse(field,Set.empty) ++ Set[HeapNode](NullHeapNode)))))
                }
              }) // update the heap
              // return the current state with updated heap
              copy(heap = heapMap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x.f := y`
              val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the right identifier
              val rcvSet = evaluatePath(path) - NullHeapNode // set of path receivers (excluding null)
              val heapMap = rcvSet.foldLeft(heap)((map, node) => {
                if (node.representsSingleVariable) { // strong update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field -> r)))
                } else { // weak update
                  map + (node -> (map.getOrElse(node,Map.empty) + (field ->
                    (map.getOrElse(node,Map.empty).getOrElse(field,Set.empty) ++ r))))
                }
              }) // update the heap
              // return the current state with updated heap
              copy(heap = heapMap).pruneUnreachableHeap()

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
              val objR = evaluatePath(right) // set of (right) receivers
            val r = objR.foldLeft(Set.empty[HeapNode])(
                (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
              ) // set of heap nodes pointed to by the right path
              // return the current state with updated store
              copy(store = store + (x -> r)).pruneUnreachableHeap()

            case right: HeapNode => // e.g., `x = new()`
              // create fresh heap node to update the path associated with right
              val fresh = HeapNode(List[Identifier](x),right.pp)
              // add x -> right to store map
              val storeMap = store + (x -> Set[HeapNode](fresh))
              // update the heap to update the path associated with right
              val fieldMap = heap.getOrElse(right, Map.empty)
              var heapMap = heap - right
              heapMap = heapMap + (fresh -> fieldMap)
              //val heapMap = heap - right + (fresh -> fieldMap)
              // return the current state with updated store and heap
              copy(store = storeMap, heap = heapMap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x := y`
              // add x -> store[right] to store map
              val storeMap = store + (x -> this.store.getOrElse(right, Set[HeapNode](NullHeapNode)))
              // return the current state with updated store
              copy(store = storeMap).pruneUnreachableHeap()

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
      case cond:Constant => this // Constant
      case cond: Identifier => this // Identifier
      case cond: BinaryArithmeticExpression => this// BinaryArithmeticExpression
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, typ) => // BinaryBooleanExpression
        this.assume(left).assume(right)
      case BinaryBooleanExpression(left, right, BooleanOperator.||, typ) => // BinaryBooleanExpression
        this.assume(left) lub this.assume(right)
      case cond: NegatedBooleanExpression => // NegatedBooleanExpression
        cond.exp match {
          case c: Constant => this // Constant
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
            val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the right identifier
            if (r.contains(NullHeapNode)) {
              copy(store = store + (right -> Set(NullHeapNode)))
            } else this.bottom() // there is no common heap node
          case (Constant("null",_,_), AccessPathIdentifier(right)) => // e.g., null == y.f
            val objR = evaluatePath(right) // set of (right) receivers
            val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
            if (r.contains((NullHeapNode))) {
              val heapMap = objR.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> Set(NullHeapNode))))
              } // update the heap pointed to by the right path
              copy(heap = heapMap).pruneUnreachableHeap()
            } else this.bottom() // there is no common heap node
          case (left: VariableIdentifier, Constant("null",_,_)) => // e.g., x == null
            val l = store.getOrElse(left, Set.empty) // set heap nodes pointed to by the left identifier
            if (l.contains(NullHeapNode)) {
              copy(store = store + (left -> Set[HeapNode](NullHeapNode)))
            } else this.bottom() // there is no common heap node
          case (left: VariableIdentifier, right: VariableIdentifier) => // e.g, x == y
            val l = store.getOrElse(left, Set.empty) // set heap nodes pointed to by the left identifier
            val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the right identifier
            val intersection = l intersect r
            if (intersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common heap node
              copy(store = store + (left -> intersection, right -> intersection)).pruneUnreachableHeap()
            }
          case (left: VariableIdentifier, AccessPathIdentifier(right)) => // e.g., x == y.f
            val l = store.getOrElse(left, Set.empty) // set heap nodes pointed to by the left identifier
            val objR = evaluatePath(right) // set of (right) receivers
            val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
            val intersection = l intersect r
            if (intersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
            val heapMap = objR.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> intersection)))
              } // update the heap pointed to by the right path
              copy(store = store + (left -> intersection), heap = heapMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), Constant("null",_,_)) => // e.g., x.f == null
            val objL = evaluatePath(left) // set of (left) receivers
            val l = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the left path
            if (l.contains((NullHeapNode))) {
              val heapMap = objL.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> Set(NullHeapNode))))
              } // update the heap pointed to by the left path
              copy(heap = heapMap).pruneUnreachableHeap()
            } else this.bottom() // there is no common heap node
          case (AccessPathIdentifier(left), right: VariableIdentifier) => // e.g., x.f == y
            val objL = evaluatePath(left) // set of (left) receivers
            val l = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the left path
            val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the right identifier
            val intersection = l intersect r
            if (intersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
            val heapMap = objL.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> intersection)))
              } // update the heap pointed to by the left path
              copy(store = store + (right -> intersection), heap = heapMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), AccessPathIdentifier(right)) => // e.g., x.f == y.f
            val objL = evaluatePath(left) // set of (left) receivers
            val objR = evaluatePath(right) // set of (right) receivers
            val l = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set heap nodes pointed to by the left path
            val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
            val intersection = l intersect r
            if (intersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
            var heapMap = objL.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> intersection)))
              } // update the heap pointed to by the left path
              heapMap = objR.foldLeft(heapMap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> intersection)))
              } // update the heap pointed to by the right path
              copy(heap = heapMap).pruneUnreachableHeap()
            }
        }
      case ReferenceComparisonExpression(left, right, ArithmeticOperator.!=, typ) =>
        (left, right) match {
          case (Constant("null", _, _), right: VariableIdentifier) => // e.g., null != y
            val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the right identifier
          val intersection = Set[HeapNode](NullHeapNode) intersect r
            val difference = r diff intersection
            if (difference.isEmpty && intersection.size == 1) this.bottom() // there is no different heap node
            else {
              // there is at least one different heap node
              copy(store = store + (right -> difference)).pruneUnreachableHeap()
            }
          case (Constant("null", _, _), AccessPathIdentifier(right)) => // e.g., null != y.f
            val objR = evaluatePath(right) // set of (right) receivers
          val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
          val intersection = Set[HeapNode](NullHeapNode) intersect r
            val difference = r diff intersection
            if (difference.isEmpty && intersection.size == 1) this.bottom() // there is no different heap node
            else {
              // there is at least one different heap node
              val heapMap = objR.foldLeft(heap) {
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> difference)))
              } // update the heap pointed to by the right path
              copy(heap = heapMap).pruneUnreachableHeap()
            }
          case (left: VariableIdentifier, Constant("null", _, _)) => // e.g., x != null
            val l = store.getOrElse(left, Set.empty) // set heap nodes pointed to by the left identifier
          val intersection = l intersect Set[HeapNode](NullHeapNode)
            val difference = l diff intersection
            if (difference.isEmpty && intersection.size == 1) this.bottom() // there is no different heap node
            else {
              // there is at least one different heap node
              copy(store = store + (left -> difference)).pruneUnreachableHeap()
            }
          case (left: VariableIdentifier, right: VariableIdentifier) => // e.g, x != y
            val l = store.getOrElse(left, Set.empty) // set heap nodes pointed to by the left identifier
          val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the right identifier
          val intersection = l intersect r
            val ldifference = l diff intersection
            val rdifference = r diff intersection
            if (ldifference.isEmpty && rdifference.isEmpty && intersection.size == 1) this.bottom()
            else {
              // there is at least one different heap node
              copy(store = store +(left -> ldifference, right -> rdifference)).pruneUnreachableHeap()
            }
          case (left: VariableIdentifier, AccessPathIdentifier(right)) => // e.g., x != y.f
            val l = store.getOrElse(left, Set.empty) // set heap nodes pointed to by the left identifier
          val objR = evaluatePath(right) // set of (right) receivers
          val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
          val intersection = l intersect r
            val ldifference = l diff intersection
            val rdifference = r diff intersection
            if (ldifference.isEmpty && rdifference.isEmpty && intersection.size == 1) this.bottom()
            else {
              // there is at least one different heap node
              val heapMap = objR.foldLeft(heap) {
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> intersection)))
              } // update the heap pointed to by the right path
              copy(store = store + (left -> ldifference), heap = heapMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), Constant("null", _, _)) => // e.g., x.f == null
            val objL = evaluatePath(left) // set of (left) receivers
          val l = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the left path
          val intersection = l intersect Set[HeapNode](NullHeapNode)
            val difference = l diff intersection
            if (difference.isEmpty && intersection.size == 1) this.bottom() // there is no different heap node
            else {
              // there is at least one different heap node
              val heapMap = objL.foldLeft(heap) {
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> difference)))
              } // update the heap pointed to by the right path
              copy(heap = heapMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), right: VariableIdentifier) => // e.g., x.f != y
            val objL = evaluatePath(left) // set of (left) receivers
          val l = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the left path
          val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the right identifier
          val intersection = l intersect r
            val ldifference = l diff intersection
            val rdifference = r diff intersection
            if (ldifference.isEmpty && rdifference.isEmpty && intersection.size == 1) this.bottom()
            else {
              // there is at least one different heap node
              val heapMap = objL.foldLeft(heap) {
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> intersection)))
              } // update the heap pointed to by the left path
              copy(store = store + (right -> rdifference), heap = heapMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), AccessPathIdentifier(right)) => // e.g., x.f != y.f
            val objL = evaluatePath(left) // set of (left) receivers
          val l = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the left path
          val objR = evaluatePath(right) // set of (right) receivers
          val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
          val intersection = l intersect r
            val ldifference = l diff intersection
            val rdifference = r diff intersection
            if (ldifference.isEmpty && rdifference.isEmpty && intersection.size == 1) this.bottom()
            else {
              // there is at least one different heap node
              var heapMap = objL.foldLeft(heap) {
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> intersection)))
              } // update the heap pointed to by the left path
              heapMap = objR.foldLeft(heapMap) {
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> intersection)))
              } // update the heap pointed to by the right path
              copy(heap = heapMap).pruneUnreachableHeap()
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
  override def bottom(): T = copy(isBottom = true)

  def copy(fields: Set[(Type, String)] = fields,
           currentPP: ProgramPoint = currentPP,
           materialization: Boolean = materialization,
           result: ExpressionSet = result,
           store: Map[VariableIdentifier,Set[HeapNode]] = store,
           heap: Map[HeapNode,Map[String, Set[HeapNode]]] = heap,
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
    // update heap map with the fresh node
    var fieldMap = Map.empty[String,Set[HeapNode]]
    for (f <- fields) { // for all fields declared within the program...
      f._1 match {
        case _:RefType => fieldMap = fieldMap + (f._2 -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
      }
    }
    val heapMap = heap + (fresh -> fieldMap)
    // return the current state with updated result, store, heap
    copy(result = ExpressionSet(fresh), heap = heapMap)
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
      copy(result = ExpressionSet(x), store = store + (x -> Set(NullHeapNode)))
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
    val storeMap = store + (x -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
      // return the current state with updated result and store
      copy(result = ExpressionSet(x), store = storeMap)
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

  /** Evaluates a path of object fields
    *
    * @param path the object fields path to evaluate
    * @return the set of objects referenced by the path (except the last field)
    */
  def evaluatePath(path: List[Identifier]) : Set[HeapNode] = {
    val first = store(path.head.asInstanceOf[VariableIdentifier]) // path head evaluation
    val eval = path.drop(1).dropRight(1).foldLeft(first)( // path tail evaluation
        (set,next) => { // next path segment
          if (set.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP)
          set.foldLeft(Set.empty[HeapNode])(
            (s,obj) => s ++ heap.getOrElse(obj,Map.empty).getOrElse(next.getName,Set.empty)
          )}
      ) // return the objects referenced by the path (except the last field)
    if (eval.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP); eval
  }

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  override def exhale(acc: Expression): T = {
    logger.trace("*** exhale(" + acc.toString + ")")

    acc match {
      case acc: PermissionExpression => {
        acc.id match {
          case AccessPathIdentifier(path) =>
            val obj = evaluatePath(path) // set of (path) receivers
          val heapMap = obj.foldLeft(heap) {
              case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (path.last.getName -> heap.keySet)))
            } // havoc the heap pointed to by the path
            copy(heap = heapMap).pruneUnreachableHeap()
          case _ => throw new IllegalArgumentException("A permission exhale must occur via an Access Path Identifier")
        }
      }
      case _ => // assert
        val asserted: T = this.setExpression(ExpressionSet(acc))
        assert(asserted.testFalse() lessEqual this.bottom())
        this.assume(acc)
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

    obj match {
      case AccessPathIdentifier(path) =>
        var storeMap: Map[VariableIdentifier, Set[HeapNode]] = store // new store map (initially equal to store)
      var heapMap: Map[HeapNode, Map[String, Set[HeapNode]]] = heap // new heap map (initially equal to heap)
      // path head evaluation
      val head = path.head.asInstanceOf[VariableIdentifier]
        var rcvSet: Set[HeapNode] = storeMap.getOrElse(head, Set.empty)  // initial set of (path) receivers
        if (rcvSet.contains(SummaryHeapNode) && materialization) { // materialization
        val fresh = HeapNode(List[Identifier](head)) // create fresh heap node
          rcvSet = rcvSet - SummaryHeapNode + fresh // update receiver set
          storeMap = storeMap + (head -> rcvSet) // add key to store map to replace the summary node with the fresh node
          heapMap = heapMap + (fresh -> heapMap(SummaryHeapNode)) // update heap map with the fresh node
        }
        // path tail evaluation
        val eval: Set[HeapNode] = path.drop(1).dropRight(1).foldLeft(rcvSet)(
          (rcv: Set[HeapNode],id: Identifier) => {  // for all following path segments...
            if (rcv.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP)
            rcv.foldLeft(Set.empty[HeapNode])(
              (set: Set[HeapNode],node: HeapNode) => {  // for all current receivers...
              var curr: Set[HeapNode] = heapMap.getOrElse(node,Map.empty).getOrElse(id.getName,Set.empty)
                if (curr.contains(SummaryHeapNode) && materialization) { // materialization
                val fresh = HeapNode(node.id :+ id) // create fresh heap node
                  curr = curr - SummaryHeapNode + fresh // update the current receiver set
                  // add key to heap map to replace the summary node with the fresh node
                  heapMap = heapMap + (node -> (heapMap.getOrElse(node,Map.empty) + (id.getName -> curr)))
                  heapMap = heapMap + (fresh -> heapMap(SummaryHeapNode)) // update heap map with the fresh node
                }
                set ++ curr
              })
          })
        if (eval.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP)
        // path end evaluation
        if (typ.isObject) { // the accessed field is a Ref
        val last = eval.foldLeft(Set.empty[HeapNode])(
            (set: Set[HeapNode],node: HeapNode) => {  // for all remaining receivers...
            var curr: Set[HeapNode] = heapMap.getOrElse(node,Map.empty).getOrElse(field,Set.empty)
              if (curr.contains(SummaryHeapNode) && materialization) { // materialization
              val fresh = HeapNode(node.id :+ path.last) // create fresh heap node
                curr = curr - SummaryHeapNode + fresh // update the current receiver set
                // add key to heap map to replace the summary node with the fresh node
                heapMap = heapMap + (node -> (heapMap.getOrElse(node,Map.empty) + (field -> curr)))
                heapMap = heapMap + (fresh -> heapMap(SummaryHeapNode)) // update heap map with the fresh node
              }
              set ++ curr
            }
          )
          if (last.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP)
        }
        // return the current state with updated result, store, heap
        copy(result = ExpressionSet(obj), store = storeMap, heap = heapMap)
      case _ => throw new IllegalArgumentException("A field access must occur via an AccessPathIdentifier")
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
    var rcvSet = store.getOrElse(x,Set.empty) // set of (path) receivers
    if (rcvSet.contains(SummaryHeapNode) && materialization) { // materialization
    val fresh = HeapNode(List[Identifier](id)) // create fresh heap node
      rcvSet = rcvSet - SummaryHeapNode + fresh // update receiver set
      val storeMap = store + (x -> rcvSet) // add key to store map to replace the summary node with the fresh node
      val heapMap = heap + (fresh -> heap(SummaryHeapNode)) // update heap map with the fresh node
      // return the current state with updated result, store, heap
      copy(result = ExpressionSet(id), store = storeMap, heap = heapMap)
    } else { copy(result = ExpressionSet(id)) } // return the current state with updated result
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: T): T = ???

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  override def inhale(acc: Expression): T = {
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

    if (isBottom || other.isTop) return true
    if (other.isBottom || isTop) return false

    val storeMap = this.store.forall {
      case (k: VariableIdentifier,s: Set[HeapNode]) => {
        val oo = other.store.getOrElse(k,Set.empty)
        (!oo.contains(SummaryHeapNode) || s.contains(SummaryHeapNode)) &&
          ((s - SummaryHeapNode) subsetOf (oo - SummaryHeapNode))
      }
    } // compare the stores
    val heapMap = this.heap.forall {
        case (o: HeapNode, m: Map[String,Set[HeapNode]]) => m.forall {
          case (f: String, s: Set[HeapNode]) => {
            val oo = other.heap.getOrElse(o,Map.empty).getOrElse(f,Set.empty)
            (!oo.contains(SummaryHeapNode) || s.contains(SummaryHeapNode)) &&
              ((s - SummaryHeapNode) subsetOf (oo - SummaryHeapNode))
          }
        }
      } // compare the heaps
    storeMap && heapMap
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: T): T = {
    logger.trace("*** lub(" + this.repr + ", " + other.repr + ")")

    def zipper[K](map1: Map[K,Set[HeapNode]], map2: Map[K,Set[HeapNode]]): Map[K,Set[HeapNode]] = {
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
    val fieldSet = this.fields ++ other.fields  // join the fieldSets
    val allowed = this.materialization || other.materialization // join the materialization flags
    val expr = this.result lub other.result // join the exprSets
    val storeMap = zipper[VariableIdentifier](this.store,other.store)  // merge the stores
    // merge the heaps
    var heapMap = Map.empty[HeapNode,Map[String,Set[HeapNode]]]
    for (key <- this.heap.keySet ++ other.heap.keySet) { // for all keys present in either map...
      (this.heap.get(key),other.heap.get(key)) match {
        case (None,None) =>
        case (None,Some(m2)) => heapMap = heapMap + (key -> m2)
        case (Some(m1),None) => heapMap = heapMap + (key -> m1)
        case (Some(m1: Map[String,Set[HeapNode]]),Some(m2: Map[String,Set[HeapNode]])) =>
          heapMap = heapMap + (key -> zipper[String](m1,m2))
      }
    }
    // return the current state with updated result, store, heap
    copy(fields = fieldSet, currentPP = DummyProgramPoint, materialization = allowed, result = expr, store = storeMap,
      heap = heapMap, isBottom = this.isBottom && other.isBottom, isTop = this.isTop || other.isTop)
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): T = {
    // retrieve all heap nodes reachable from Ref variables
    var reach = store.foldLeft(Set[HeapNode]())((r, s) => r ++ s._2)
    // add all heap nodes reachable from fields of heap nodes reachable from Ref variables
    reach = reach ++ reach.foldLeft(Set.empty[HeapNode])(
      (set, node) => set ++ heap.getOrElse(node,Map.empty).foldLeft(Set.empty[HeapNode])((r,s) => r ++ s._2)
    )
    // remove all unreachable heap nodes
    var heapMap = heap; for (key <- heap.keySet diff reach) { heapMap = heapMap - key }
    // return the current state with updated heap
    copy(heap = heapMap)
  }

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = ???

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
  def repr: String = s"AliasAnalysisState( $result, $store, $heap )"

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
  override def toString: String = s"AliasAnalysisState(\n\tresult: $result\n\tstore: $store\n\theap: $heap\n)"

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): T = copy(isTop = true)

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: T): T = {
    logger.trace("*** ----------------widening(" + other.repr + ")")

    def zipper[K](map1: Map[K,Set[HeapNode]], map2: Map[K,Set[HeapNode]]): Map[K,Set[HeapNode]] = {
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
    val fieldSet = this.fields ++ other.fields  // join the fieldSets
    val expr = this.result widening other.result // join the exprSets
    val storeMap = zipper[VariableIdentifier](this.store,other.store)  // merge the stores
    // merge the heaps
    var heapMap = Map.empty[HeapNode,Map[String,Set[HeapNode]]]
    for (key <- this.heap.keySet ++ other.heap.keySet) { // for all keys present in either map...
      (this.heap.get(key),other.heap.get(key)) match {
        case (None,None) =>
        case (None,Some(m2)) => heapMap = heapMap + (key -> m2)
        case (Some(m1),None) => heapMap = heapMap + (key -> m1)
        case (Some(m1: Map[String,Set[HeapNode]]),Some(m2: Map[String,Set[HeapNode]])) =>
          heapMap = heapMap + (key -> zipper[String](m1,m2))
      }
    }
    // return the current state with updated result, store, heap
    copy(fields = fieldSet, currentPP = DummyProgramPoint, materialization = false, result = expr, store = storeMap,
      heap = heapMap, isBottom = this.isBottom && other.isBottom, isTop = this.isTop || other.isTop)
  }
}

object AliasAnalysisState {
  case class Default(fields: Set[(Type, String)] = Set.empty,
                     currentPP: ProgramPoint = DummyProgramPoint,
                     materialization: Boolean = true,
                     result: ExpressionSet = ExpressionSet(),
                     store: Map[VariableIdentifier, Set[HeapNode]] = Map.empty,
                     heap: Map[HeapNode, Map[String, Set[HeapNode]]] = Map.empty,
                     isBottom: Boolean = false,
                     isTop: Boolean = false)
    extends AliasAnalysisState[Default] {

    override def copy(fields: Set[(Type, String)],
                      currentPP: ProgramPoint,
                      materialization: Boolean,
                      result: ExpressionSet,
                      store: Map[VariableIdentifier, Set[HeapNode]],
                      heap: Map[HeapNode, Map[String, Set[HeapNode]]],
                      isBottom: Boolean,
                      isTop: Boolean): Default =
      Default(fields, currentPP, materialization, result, store, heap, isBottom, isTop)
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
    // prepare the initial heap map
    var fieldMap = Map.empty[String,Set[HeapNode]]
    for (f <- fields) { // for all fields declared within the program...
      f._1 match {
        case _:RefType => fieldMap = fieldMap + (f._2 -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
      }
    }
    val heapMap = Map[HeapNode, Map[String, Set[HeapNode]]](SummaryHeapNode -> fieldMap) // add key to heap map
    // initialize the entry state
    method.initializeArgument[T](topState.copy(fields = fields, heap = heapMap))
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

/**
  * ATTEMPT AT FORWARD+BACKWARD ANALYSIS
  */

trait AttemptState[A <: AliasAnalysisState[A], T <: AttemptState[A,T]]
  extends SimplePermissionState[T] with PreviousResult[A, T]
  with StateWithRefiningAnalysisStubs[T]
  with LazyLogging {
  this: T =>

  val alias: TrackingCFGState[A]

  def copy(alias: TrackingCFGState[A] = alias): T

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  override def inhale(acc: Expression): T = ???

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  override def exhale(acc: Expression): T = ???

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): T = ???

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    */
  override def removeVariable(varExpr: VariableIdentifier): T = ???

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
  override def getFieldValue(obj: Expression, field: String, typ: Type): T = ???

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): T = ???

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): T = ???

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  override def assignVariable(x: Expression, right: Expression): T = ???

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  override def setVariableToTop(varExpr: Expression): T = ???

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  override def assignField(obj: Expression, field: String, right: Expression): T = ???

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): T = ???

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression
    */
  override def removeExpression(): T = this

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    */
  override def throws(t: ExpressionSet): T = ???

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = ???

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = this

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): T = ???

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): T = ???

  /** Returns the current expression. */
  override def expr: ExpressionSet = ExpressionSet()

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): T = ???

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): T = this

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): T = this

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): T = this

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: T): T = ???

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: T): Boolean = false

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): T = ???

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: T): T = other

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): T = ???

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: T): T = ???

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return bottom
    */
  override def isBottom: Boolean = false

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return bottom
    */
  override def isTop: Boolean = ???

  override def addPreviousResult(result: TrackingCFGState[A]): T = copy(alias = result)
}

object AttemptState {
  case class Default(alias: TrackingCFGState[AliasAnalysisState.Default] = null)
    extends AttemptState[AliasAnalysisState.Default,Default] {

    override def copy(alias: TrackingCFGState[AliasAnalysisState.Default]): Default = Default(alias)
  }
}

object AttemptEntryState extends BackwardEntryStateBuilder[AttemptState.Default] {
  override def topState: AttemptState.Default = AttemptState.Default()
}

trait AttemptRunner[A <: AliasAnalysisState[A], T <: AttemptState[A, T]] extends SilverAnalysisRunner[T] {

}

object Attempt extends AttemptRunner[AliasAnalysisState.Default,AttemptState.Default] {
  override val analysis =
    SimpleForwardBackwardAnalysis[AliasAnalysisState.Default,AttemptState.Default](AliasAnalysisEntryState,AttemptEntryState)
  override def toString = "Attempt"
}