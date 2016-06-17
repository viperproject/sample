package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.execution.{EntryStateBuilder, SimpleBackwardAnalysis, SimpleForwardAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{RefType, SilverAnalysisRunner}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Statement, Type}
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging


/** Heap node.
  *
  * @param number the unique identifier of the heap node
  * @author Caterina Urban
  */
case class HeapNode(number: Int) extends Identifier
{
  override def getName: String = "0" + number
  override def equals(o: Any) = o match {
    case that: HeapNode => this.getName equals that.getName
    case _ => false
  }
  override def hashCode = getName.hashCode
  override def getField: Option[String] = None
  override def representsSingleVariable: Boolean = true
  override def pp: ProgramPoint = DummyProgramPoint
  override def typ: Type = DummyRefType
  override def toString: String = "O" + number
}

/** Unique heap summary node.
  *
  * @author Caterina Urban
  */
object SummaryHeapNode extends HeapNode(0) {
  override def representsSingleVariable: Boolean = false
  override def toString: String = "Σ"
}

/** Null heap node.
  *
  * @author Caterina Urban
  */
object NullHeapNode extends HeapNode(-1) {
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
  
  // result of the previous statement (sort of)
  def result: ExpressionSet

  // set of fields declared in the program
  def fields: Set[(Type, String)]
  
  // map from Ref variables to heap objects
  def store: Map[VariableIdentifier,Set[HeapNode]]
  
  // map from heap objects to a map from Ref fields to heap objects
  def heap: Map[HeapNode,Map[String, Set[HeapNode]]]
  
  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  override def exhale(acc: Expression): T = ???

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  override def inhale(acc: Expression): T = ???

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
      if (heap.contains(SummaryHeapNode)) { // the summary heap node exists already
      // add key to store map
      val refMap = store + (x -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
        // return the current state with updated store and heap
        this.copy(result = ExpressionSet(x), store = refMap)
      } else { // the summary heap node was never created before
      // prepare fields to add to heap map and add variables to numDom
      var fieldMap = Map[String,Set[HeapNode]]()
        for (f <- fields) { // for all fields declared within the program...
          f._1 match {
            case _:RefType =>
              fieldMap = fieldMap + (f._2 -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
          }
        }
        // add key to store map
        val refMap = store + (x -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
        // add key to heap map
        val objMap = heap + (SummaryHeapNode -> fieldMap)
        // return the current state with updated exprSet, store, heap and numDom
        this.copy(result = ExpressionSet(x), store = refMap, heap = objMap)
      }
    } else this // the variable to be created is not a Ref
  }

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
            val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the left identifier
            if (r.contains(NullHeapNode)) {
              copy(store = store + (right -> Set(NullHeapNode)))
            } else this.bottom() // there is no common heap node
          case (Constant("null",_,_), AccessPathIdentifier(right)) => // e.g., null == y.f
            val objR = evaluatePath(right) - NullHeapNode // set of (right) receivers
            val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
            if (r.contains((NullHeapNode))) {
              val objMap = objR.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> Set(NullHeapNode))))
              } // update the heap pointed to by the right path
              copy(heap = objMap).pruneUnreachableHeap()
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
            val objR = evaluatePath(right) - NullHeapNode // set of (right) receivers
            val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
            val intersection = l intersect r
            if (intersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
              val objMap = objR.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> intersection)))
              } // update the heap pointed to by the right path
              copy(store = store + (left -> intersection), heap = objMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), Constant("null",_,_)) => // e.g., x.f == null
            val objL = evaluatePath(left) - NullHeapNode // set of (left) receivers
            val r = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the left path
            if (r.contains((NullHeapNode))) {
              val objMap = objL.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> Set(NullHeapNode))))
              } // update the heap pointed to by the left path
              copy(heap = objMap).pruneUnreachableHeap()
            } else this.bottom() // there is no common heap node
          case (AccessPathIdentifier(left), right: VariableIdentifier) => // e.g., x.f == y
            val objL = evaluatePath(left) - NullHeapNode // set of (left) receivers
            val l = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the left path
            val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the right identifier
            val intersection = l intersect r
            if (intersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
              val objMap = objL.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> intersection)))
              } // update the heap pointed to by the right path
              copy(store = store + (right -> intersection), heap = objMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), AccessPathIdentifier(right)) => // e.g., x.f == y.f
            val objL = evaluatePath(left) - NullHeapNode // set of (left) receivers
            val objR = evaluatePath(right) - NullHeapNode // set of (right) receivers
            val l = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set heap nodes pointed to by the left path
            val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
            val intersection = l intersect r
            if (intersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
              var objMap = objL.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> intersection)))
              } // update the heap pointed to by the left path
              objMap = objR.foldLeft(objMap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> intersection)))
              } // update the heap pointed to by the right path
              copy(heap = objMap).pruneUnreachableHeap()
            }
        }
      case ReferenceComparisonExpression(left, right, ArithmeticOperator.!=, typ) => ???
        /*
         (left, right) match {
          case (Constant("null",_,_), right: VariableIdentifier) => // e.g., null == y
            val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the left identifier
            if (r.contains(NullHeapNode)) {
              copy(store = store + (right -> Set(NullHeapNode)))
            } else this.bottom() // there is no common heap node
          case (Constant("null",_,_), AccessPathIdentifier(right)) => // e.g., null == y.f
            val objR = evaluatePath(right) - NullHeapNode // set of (right) receivers
            val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
            if (r.contains((NullHeapNode))) {
              val objMap = objR.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> Set(NullHeapNode))))
              } // update the heap pointed to by the right path
              copy(heap = objMap).pruneUnreachableHeap()
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
            val objR = evaluatePath(right) - NullHeapNode // set of (right) receivers
            val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
            val intersection = l intersect r
            if (intersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
              val objMap = objR.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> intersection)))
              } // update the heap pointed to by the right path
              copy(store = store + (left -> intersection), heap = objMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), Constant("null",_,_)) => // e.g., x.f == null
            val objL = evaluatePath(left) - NullHeapNode // set of (left) receivers
            val r = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the left path
            if (r.contains((NullHeapNode))) {
              val objMap = objL.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> Set(NullHeapNode))))
              } // update the heap pointed to by the left path
              copy(heap = objMap).pruneUnreachableHeap()
            } else this.bottom() // there is no common heap node
          case (AccessPathIdentifier(left), right: VariableIdentifier) => // e.g., x.f == y
            val objL = evaluatePath(left) - NullHeapNode // set of (left) receivers
            val l = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the left path
            val r = store.getOrElse(right, Set.empty) // set heap nodes pointed to by the right identifier
            val intersection = l intersect r
            if (intersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
              val objMap = objL.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> intersection)))
              } // update the heap pointed to by the right path
              copy(store = store + (right -> intersection), heap = objMap).pruneUnreachableHeap()
            }
          case (AccessPathIdentifier(left), AccessPathIdentifier(right)) => // e.g., x.f == y.f
            val objL = evaluatePath(left) - NullHeapNode // set of (left) receivers
            val objR = evaluatePath(right) - NullHeapNode // set of (right) receivers
            val l = objL.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(left.last.getName, Set.empty)
            ) // set heap nodes pointed to by the left path
            val r = objR.foldLeft(Set.empty[HeapNode])(
              (set, id) => set ++ heap.getOrElse(id, Map.empty).getOrElse(right.last.getName, Set.empty)
            ) // set of heap nodes pointed to by the right path
            val intersection = l intersect r
            if (intersection.isEmpty) this.bottom() // there is no common heap node
            else { // there is at least one common node
              var objMap = objL.foldLeft(heap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (left.last.getName -> intersection)))
              } // update the heap pointed to by the left path
              objMap = objR.foldLeft(objMap){
                case (map, id) => map + (id -> (map.getOrElse(id, Map.empty) + (right.last.getName -> intersection)))
              } // update the heap pointed to by the right path
              copy(heap = objMap).pruneUnreachableHeap()
            }
        }
         */

//      case ReferenceComparisonExpression(left, right, ArithmeticOperator.!=, typ) =>
//        (left, right) match {
//          case (left: Identifier, right: Identifier) =>
//            val l = left match {
//              case left: VariableIdentifier => store.getOrElse(left,Set[HeapNode]())
//              case left: HeapAccess => heap.getOrElse(left.rcv,Map[String,Set[HeapNode]]()).
//                getOrElse(left.field,Set[HeapNode]())
//              case _ => Set[HeapNode]()
//            }
//            val r = right match {
//              case right: VariableIdentifier => store.getOrElse(right,Set[HeapNode]())
//              case right: HeapAccess => heap.getOrElse(right.rcv,Map[String,Set[HeapNode]]()).
//                getOrElse(right.field,Set[HeapNode]())
//              case _ => Set[HeapNode]()
//            }
//            val intersection = l intersect r
//            val lr = l diff intersection
//            val rl = r diff intersection
//            if (lr.isEmpty && rl.isEmpty && intersection.size == 1 && intersection.head.representsSingleVariable)
//              this.bottom() // return the bottom state
//            else { // there is at least one different heap node
//            var refMap = store
//              refMap = left match {
//                case left: VariableIdentifier => refMap + (left -> lr)
//                case _ => refMap
//              }
//              refMap = right match {
//                case right: VariableIdentifier => refMap + (right -> rl)
//                case _ => refMap
//              }
//              var objMap = heap
//              objMap = left match {
//                case left: HeapAccess => objMap + (left.rcv -> Map[String,Set[HeapNode]](left.field -> lr))
//                case _ => objMap
//              }
//              objMap = right match {
//                case right: HeapAccess => objMap + (right.rcv -> Map[String,Set[HeapNode]](right.field -> rl))
//                case _ => objMap
//              }
//              // return the current state with updated store and heap
//              this.copy(store = refMap, heap = objMap).pruneUnreachableHeap()
//            }
//          case (left: Identifier, Constant("null",_,_)) =>
//            val l = left match {
//              case left: VariableIdentifier => store.getOrElse(left,Set[HeapNode]())
//              case left: HeapAccess => heap.getOrElse(left.rcv,Map[String,Set[HeapNode]]()).
//                getOrElse(left.field,Set[HeapNode]())
//              case _ => Set[HeapNode]()
//            }
//            val r = Set[HeapNode](NullHeapNode$)
//            val intersection = l intersect r
//            val lr = l diff intersection
//            if (lr.isEmpty && intersection.size == 1 && intersection.head.representsSingleVariable)
//              this.bottom()
//            else {
//              val refMap = left match {
//                case left: VariableIdentifier => store + (left -> lr)
//                case _ => store
//              }
//              this.copy(store = refMap).pruneUnreachableHeap() // return the current state with updated store
//            }
//          case (Constant("null",_,_), right: Identifier) =>
//            val l = Set[HeapNode](NullHeapNode$)
//            val r = right match {
//              case right: VariableIdentifier => store.getOrElse(right,Set[HeapNode]())
//              case right: HeapAccess => heap.getOrElse(right.rcv,Map[String,Set[HeapNode]]()).
//                getOrElse(right.field,Set[HeapNode]())
//              case _ => Set[HeapNode]()
//            }
//            val intersection = l intersect r
//            val rl = r diff intersection
//            if (rl.isEmpty && intersection.size == 1 && intersection.head.representsSingleVariable)
//              this.bottom()
//            else {
//              val refMap = right match {
//                case right: VariableIdentifier => store + (right -> rl)
//                case _ => store
//              }
//              this.copy(store = refMap).pruneUnreachableHeap() // return the current state with updated store
//            }
//        }

      case _ => throw new NotImplementedError("An assume implementation is missing.")
    }
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
  override def removeExpression(): T = ???

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
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    // return the current state with updated result
    this.copy(result = ExpressionSet(new Constant(value, typ, pp)))
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): T = ???

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): T = this // TODO: Implement me, please :)

  /** Returns the current expression. */
  override def expr: ExpressionSet = this.result

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
  override def setExpression(expr: ExpressionSet): T = ???

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): T = ???

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): T = ???

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
  override def lessEqual(other: T): Boolean = ???

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
  override def lub(other: T): T = ???

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

  /** Evaluates a path of object fields
    *
    * @param path the object fields path to evaluate
    * @return the set of objects referenced by the path (except the last field)
    */
  def evaluatePath(path: List[Identifier]) : Set[HeapNode] = {
    val keys = store.keySet // set of all Ref variables

    val first = store(path.head.asInstanceOf[VariableIdentifier]) // set of objects pointed by the head of the path
    // path evaluation
    val ids = path.drop(1).dropRight(1).foldLeft(first)(
        (set,next) => // next path segment
          set.foldLeft(Set[HeapNode]())(
            (s,obj) => s ++ heap.getOrElse(obj,Map.empty).getOrElse(next.getName,Set.empty)
          )
      )
    // TODO: fix program point
    if (ids.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", DummyProgramPoint)
    ids // return the objects referenced by the path (except the last field)
  }

  override def toString: String = {
    "AliasAnalysisState(\n" +
      "\tresult: " + result.toString + "\n" +
      "\tstore: " + store.toString + "\n" +
      "\theap: " + heap.toString + "\n" +
      ")"
  }

  def copy(fields: Set[(Type, String)] = fields,
           result: ExpressionSet = result,
           store: Map[VariableIdentifier,Set[HeapNode]] = store,
           heap: Map[HeapNode,Map[String, Set[HeapNode]]] = heap,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): T
}

object AliasAnalysisState {
  case class Inner(fields: Set[(Type, String)] = Set.empty,
                   result: ExpressionSet = ExpressionSet(),
                   store: Map[VariableIdentifier, Set[HeapNode]] = Map.empty,
                   heap: Map[HeapNode, Map[String, Set[HeapNode]]] = Map.empty,
                   isTop: Boolean = false,
                   isBottom: Boolean = false)
    extends AliasAnalysisState[Inner] {

    override def copy(fields: Set[(Type, String)],
                      result: ExpressionSet,
                      store: Map[VariableIdentifier, Set[HeapNode]],
                      heap: Map[HeapNode, Map[String, Set[HeapNode]]],
                      isTop: Boolean = isTop,
                      isBottom: Boolean = isBottom): Inner
    = Inner(fields, result, store, heap, isTop, isBottom)
  }
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


/** Alias Analysis Entry State Builder.
  *
  * @author Jerome Dohrau, Caterina Urban
  */
object AliasAnalysisEntryStateBuilder extends EntryStateBuilder[AliasAnalysisState.Inner] {
  override def topState: AliasAnalysisState.Inner = AliasAnalysisState.Inner()
}

/** Backward Permission Inference Runner.
  *
  * @author Jerome Dohrau, Caterina Urban
  */
object AliasAnalysis extends AliasAnalysisRunner[AliasAnalysisState.Inner] {
  override val analysis = SimpleForwardAnalysis[AliasAnalysisState.Inner](AliasAnalysisEntryStateBuilder)
  override def toString = "Alias Analysis"
}