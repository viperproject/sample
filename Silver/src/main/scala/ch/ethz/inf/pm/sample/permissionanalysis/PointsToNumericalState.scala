/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron.Polyhedra
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.execution.{ForwardEntryStateBuilder, SimpleForwardAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.{LineColumnProgramPoint, _}
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging
import viper.silver.ast.LineColumnPosition

/** Object created at object allocation site.
  *
  * @param typ the type of the object
  * @param pp the object allocation site
  * @author Caterina Urban
  */
case class HeapIdentifier(typ: Type, pp: ProgramPoint) extends Identifier.HeapIdentifier {
  /** Whether the identifier represents a summary node. */
  private var summary: Boolean = false
  def setSummary(s: Boolean): HeapIdentifier = { summary=s; this }
  /** The name of the heap identifier. */
  override def getName: String = "O" + number
  private def number : String = pp match {
    case pp:LineColumnProgramPoint => pp.getLine.toString + pp.getColumn.toString
    case _ => pp.description
  }
  /** Custom equals. */
  override def equals(o: Any) = o match {
    case that: HeapIdentifier => this.getName equals that.getName
    case _ => false
  }
  /** Custom hashCode. */
  override def hashCode = getName.hashCode
  /** The name of the field represented by the identifier. */
  override def getField: Option[String] = None
  /** Whether the identifier does not represent a summary node. */
  override def representsSingleVariable: Boolean = !summary
  /** String representation of the heap identifier. */
  override def toString: String = if (summary) "Σ" + number else "O" + number
}

/** Null object.
  *
  * @author Caterina Urban
  */
object NullHeapIdentifier extends HeapIdentifier(DummyRefType, DummyProgramPoint) {
  override def getName: String = "null"
  override def toString: String = "null"
}

case object DummyRefType extends DummyType {
  def name = "Ref"
  def isBottom = false
  def isTop = false
  def isObject = true
  def isNumericalType = false
  def possibleFields: Set[Identifier] = Set.empty
}

/** Field of an object.
  *
  * @param obj the object
  * @param field the name of the field
  * @param typ the type of the field
  * @author Caterina Urban
  */
case class FieldIdentifier(obj: HeapIdentifier, field: String, typ: Type) extends Identifier.FieldIdentifier {
  /** The name of the object field. */
  override def getName: String = obj.getName + "." + field
  /** The name of the field represented by the identifier. */
  override def getField: Option[String] = Some(field)
  /** Whether the identifier is a summary node. */
  override def representsSingleVariable: Boolean = obj.representsSingleVariable
  /** Point in the program where the identifier is located. */
  override def pp: ProgramPoint = obj.pp
}

/** PointsTo+Numerical Analysis State.
  *
  * The points-to analysis is a simple allocation-site abstraction.
  * The numerical analysis uses the intervals/polyhedra abstract domain.
  *
  * @tparam T the numerical domain
  * @tparam S the pointsto+numerical state
  * @author Caterina Urban
  */
trait PointsToNumericalState[T <: NumericalDomain[T], S <: PointsToNumericalState[T,S]]
  extends SimpleState[S]
  with StateWithRefiningAnalysisStubs[S]
  with LazyLogging
{
  this: S =>

  def ids = IdentifierSet.Top
  def exprSet: ExpressionSet // `ExpressionSet` used to store the result of each statement
  def fieldSet: Set[(Type,String)] // fields declared within the program
  // map from `Ref` variables to heap objects
  def refToObj: Map[VariableIdentifier,Set[HeapIdentifier]]
  // map from heap objects to a map from `Ref` fields to heap objects
  def objFieldToObj: Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]]
  def numDom: T // intervals/polyhedra abstract domain
  def currentPP: ProgramPoint

  // retrieves all different shortest paths from which a heap identifier is reachable
  def pathFromObj(obj: HeapIdentifier): Set[(VariableIdentifier,List[String])] = {
    var paths = Set[(VariableIdentifier,List[String])]() // set of complete paths to the given heap identifier

    for (v: VariableIdentifier <- refToObj.keySet) { // for all variables...
      if (refToObj(v).contains(obj)) { // the given heap identifier is directly reachable from the variable
        paths = paths + ((v,List[String]())) // add path to the set of complete paths
      } else { // variant of breadth-first search
        var objs = objFieldToObj.keySet // set of all heap identifiers
        var queue = Set[(List[String],HeapIdentifier)]()
        for (o <- refToObj(v) - NullHeapIdentifier) { // for all heap identifiers directly reachable from the variable...
          // populate the queue with the heap identifiers directly reachable from the variable
          queue = queue + ((List[String](),o))
          objs = objs - o // remove the heap identifiers from the set of all heap identifiers
        }
        while (!queue.isEmpty) {
          val el: (List[String], HeapIdentifier) = queue.head // pop an element from the queue
          queue = queue - el
          for ((f,ss) <- objFieldToObj(el._2)) { // for all pairs of field and set of heap identifiers...
            if (ss.contains(obj)) { // the given heap identifier is directly reachable via the field
              paths = paths + ((v, f::el._1)) // add path to the set of complete paths
              // note that the path is stored in reverse order for efficiency
            } else {
              for (o <- ss - NullHeapIdentifier if objs.contains(o)) { // for all (unseen) heap identifiers directly reachable via the field...
                // populate the queue with the heap identifiers directly reachable via the field
                queue = queue + ((f::el._1,o))
                objs = objs - o // remove the heap identifiers from the set of all (remaining) heap identifiers
              }
            }
          }
        }
      }
    }

    paths
  }

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  override def assignField(obj: Expression, field: String, right: Expression): S = {
    logger.debug("*** ----------------assignField(" + obj.toString + "; " + field.toString + "; " + right.toString + ")")

    obj match {
      case obj: FieldIdentifier =>
        if (obj.typ.isObject) { // the assigned field is a `Ref`
          right match {

            case right: FieldIdentifier => // e.g., `x.f := y.g`
              val s = objFieldToObj(right.obj)(right.field) // retrieve the heap `Obj` objects
              val o = obj.obj // retrieve `Obj` whose field is assigned
              val f = obj.field // retrieve assigned field
              val objFieldToObjmap = if (o.representsSingleVariable) { // strong update
                objFieldToObj + (o -> (objFieldToObj(o) + (f -> s)))
              } else { // weak update
                objFieldToObj + (o -> (objFieldToObj(o) + (f -> (objFieldToObj(o)(f) ++ s))))
              }
              // return the current state with updated objFieldToObj
              this.copy(objFieldToObj = objFieldToObjmap).pruneUnreachableHeap()

            case right: Constant => // e.g., `x.f := null`
              val o = obj.obj // retrieve `Obj` whose field is assigned
              val f = obj.field // retrieve assigned field
              // weak update
              val objFieldToObjmap = if (o.representsSingleVariable) { // strong update
                objFieldToObj + (o -> (objFieldToObj(o) + (f -> Set[HeapIdentifier](NullHeapIdentifier))))
              } else { // weak update
                objFieldToObj + (o -> (objFieldToObj(o) + (f -> (objFieldToObj(o)(f) ++ Set[HeapIdentifier](NullHeapIdentifier)))))
              }
              // return the current state with updated objFieldToObj
              this.copy(objFieldToObj = objFieldToObjmap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x.f := y`
              val s = refToObj(right) // retrieve the corresponding heap `Obj` objects
              val o = obj.obj // retrieve `Obj` whose field is assigned
              val f = obj.field // retrieve assigned field
              // weak update
              val objFieldToObjmap = if (o.representsSingleVariable) { // strong update
                objFieldToObj + (o -> (objFieldToObj(o) + (f -> s)))
              } else { // weak update
                objFieldToObj + (o -> (objFieldToObj(o) + (f -> (objFieldToObj(o)(f) ++ s))))
              }
              // return the current state with updated objFieldToObj
              this.copy(objFieldToObj = objFieldToObjmap).pruneUnreachableHeap()

            case _ => throw new NotImplementedError("A field assignment implementation is missing.")
          }
        } else {  // the assigned field is not a `Ref`
          val num = if (obj.obj.representsSingleVariable) { // strong update
            numDom.assign(obj,right)
          } else { // weak update
            numDom lub numDom.assign(obj,right)
          }
          // return the current state with updated numDom
          this.copy(numDom = num)
        }
      case _ => throw new IllegalArgumentException("A field assignment must occur via a FieldIdentifier.")
    }
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  override def assignVariable(x: Expression, right: Expression): S = {
    logger.debug("*** ----------------assignVariable(" + x.toString + "; " + right.toString + ")")

    x match {
      case x: VariableIdentifier =>
        if (x.typ.isObject) { // the assigned variable is a `Ref`
          right match {
            case right: FieldIdentifier => // e.g., `x := y.g`
              val s = this.objFieldToObj(right.obj)(right.field) // retrieve the heap `Obj` objects
              // add xref -> s to refToObj map
              val refToObjmap = this.refToObj + (x -> s)
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap).pruneUnreachableHeap()

            case right: HeapIdentifier => // e.g., `x = new()`
              // add xref -> right to refToObj map
              val refToObjmap = this.refToObj + (x -> Set[HeapIdentifier](right))
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x := y`
              // add xref -> refToObj[rightref] to refToObj map
              val refToObjmap = this.refToObj + (x -> this.refToObj.getOrElse(right, Set[HeapIdentifier](NullHeapIdentifier)))
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap).pruneUnreachableHeap()

            case _ => throw new NotImplementedError("A variable assignment implementation is missing.")
          }
        } else {  // the assigned variable is not a `Ref`
          // return the current state with updated numDom
          this.copy(numDom = numDom.assign(x,right))
        }
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
  override def assume(cond: Expression): S = {
    logger.debug("*** ----------------assume(" + cond.toString + ")")

    cond match {
      // Constant
      case cond:Constant =>
        val num = numDom.assume(cond)
        // return the current state with updated numDom
        if (num.isBottom) this.bottom() else this.copy(numDom = num)

      // Identifier
      case cond: Identifier =>
        val num = numDom.assume(cond)
        // return the current state with updated numDom
        if (num.isBottom) this.bottom() else this.copy(numDom = num)

      // BinaryArithmeticExpression
      case cond: BinaryArithmeticExpression =>
        val num = numDom.assume(cond)
        // return the current state with updated numDom
        if (num.isBottom) this.bottom() else this.copy(numDom = num)

      // BinaryBooleanExpression
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, typ) =>
        if (cond.canonical) {
          val num = numDom.assume(cond)
          // return the current state with updated numDom
          if (num.isBottom) this.bottom() else this.copy(numDom = num)
        } else this.assume(left).assume(right)
      case BinaryBooleanExpression(left, right, BooleanOperator.||, typ) =>
        if (cond.canonical) {
          val num = numDom.assume(cond)
          // return the current state with updated numDom
          if (num.isBottom) this.bottom() else this.copy(numDom = num)
        } else this.assume(left) lub this.assume(right)

      // NegatedBooleanExpression
      case cond: NegatedBooleanExpression => {
        cond.exp match {
          // Constant
          case c: Constant =>
            val num = numDom.assume(cond)
            // return the current state with updated numDom
            if (num.isBottom) this.bottom() else this.copy(numDom = num)

          // Identifier (i.e., FieldIdentifier, VariableIdentifier)
          case id: Identifier =>
            val num = numDom.assume(cond)
            // return the current state with updated numDom
            if (num.isBottom) this.bottom() else this.copy(numDom = num)

          // BinaryArithmeticExpression
          case BinaryArithmeticExpression(left, right, op, typ) =>
            this.assume(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op), typ))

          // BinaryBooleanExpression
          case BinaryBooleanExpression(left, right, op, typ) =>
            val nleft = NegatedBooleanExpression(left)
            val nright = NegatedBooleanExpression(right)
            val nop = op match {
              case BooleanOperator.&& => BooleanOperator.||
              case BooleanOperator.|| => BooleanOperator.&&
            }
            this.assume(BinaryBooleanExpression(nleft, nright, nop, typ))

          // NegatedBooleanExpression
          case NegatedBooleanExpression(exp) => this.assume(exp)

          // ReferenceComparisonExpression
          case ReferenceComparisonExpression(left, right, op, typ) =>
            val nop = op match {
              case ArithmeticOperator.== => ArithmeticOperator.!=
              case ArithmeticOperator.!= => ArithmeticOperator.==
            }
            this.assume(ReferenceComparisonExpression(left, right, nop, typ))

          case _ => throw new NotImplementedError("An assumeNegatedBooleanExpression implementation for "
            + cond.exp.getClass.getSimpleName + " is missing.")
        }
      }

      case ReferenceComparisonExpression(left, right, ArithmeticOperator.==, typ) =>
        (left, right) match {
          case (left: Identifier, right: Identifier) =>
            val l = left match {
              case left: VariableIdentifier => refToObj.getOrElse(left,Set[HeapIdentifier]())
              case left: FieldIdentifier => objFieldToObj.getOrElse(left.obj,Map[String,Set[HeapIdentifier]]()).
                getOrElse(left.field,Set[HeapIdentifier]())
              case _ => Set[HeapIdentifier]()
            }
            val r = right match {
              case right: VariableIdentifier => refToObj.getOrElse(right,Set[HeapIdentifier]())
              case right: FieldIdentifier => objFieldToObj.getOrElse(right.obj,Map[String,Set[HeapIdentifier]]()).
                getOrElse(right.field,Set[HeapIdentifier]())
              case _ => Set[HeapIdentifier]()
            }
            val intersection = l intersect r
            if (intersection.isEmpty) { // there is no common Obj
              this.bottom() // return the bottom state
            } else { // there is at least a common Obj
              var refToObjmap = refToObj
              refToObjmap = left match {
                case left: VariableIdentifier => refToObjmap + (left -> intersection)
                case _ => refToObjmap
              }
              refToObjmap = right match {
                case right: VariableIdentifier => refToObjmap + (right -> intersection)
                case _ => refToObjmap
              }
              var objFieldToObjmap = objFieldToObj
              objFieldToObjmap = left match {
                case left: FieldIdentifier =>
                  objFieldToObjmap + (left.obj -> Map[String,Set[HeapIdentifier]](left.field -> intersection))
                case _ => objFieldToObjmap
              }
              objFieldToObjmap = right match {
                case right: FieldIdentifier =>
                  objFieldToObjmap + (right.obj -> Map[String,Set[HeapIdentifier]](right.field -> intersection))
                case _ => objFieldToObjmap
              }
              // return the current state with updated refToObj and objFieldToObj
              this.copy(refToObj = refToObjmap, objFieldToObj = objFieldToObjmap).pruneUnreachableHeap()
            }
          case (left: Identifier, Constant("null",_,_)) =>
            val l = left match {
              case left: VariableIdentifier => refToObj.getOrElse(left,Set[HeapIdentifier]())
              case left: FieldIdentifier => objFieldToObj.getOrElse(left.obj,Map[String,Set[HeapIdentifier]]()).
                getOrElse(left.field,Set[HeapIdentifier]())
              case _ => Set[HeapIdentifier]()
            }
            if (l.contains(NullHeapIdentifier)) {
              // replace key into refToObj map
              val refToObjmap = left match {
                case left: VariableIdentifier => refToObj + (left -> Set[HeapIdentifier](NullHeapIdentifier))
                case _ => refToObj
              }
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap).pruneUnreachableHeap()
            } else this.bottom() // return the bottom state
          case (Constant("null",_,_), right: Identifier) =>
            val r = right match {
              case right: VariableIdentifier => refToObj.getOrElse(right,Set[HeapIdentifier]())
              case right: FieldIdentifier => objFieldToObj.getOrElse(right.obj,Map[String,Set[HeapIdentifier]]()).
                getOrElse(right.field,Set[HeapIdentifier]())
              case _ => Set[HeapIdentifier]()
            }
            if (r.contains(NullHeapIdentifier)) {
              // replace key into refToObj map
              val refToObjmap = right match {
                case right: VariableIdentifier => refToObj + (right -> Set[HeapIdentifier](NullHeapIdentifier))
                case _ => refToObj
              }
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap).pruneUnreachableHeap()
            } else this.bottom() // return the bottom state
        }

      case ReferenceComparisonExpression(left, right, ArithmeticOperator.!=, typ) =>
        (left, right) match {
          case (left: Identifier, right: Identifier) =>
            val l = left match {
              case left: VariableIdentifier => refToObj.getOrElse(left,Set[HeapIdentifier]())
              case left: FieldIdentifier => objFieldToObj.getOrElse(left.obj,Map[String,Set[HeapIdentifier]]()).
                getOrElse(left.field,Set[HeapIdentifier]())
              case _ => Set[HeapIdentifier]()
            }
            val r = right match {
              case right: VariableIdentifier => refToObj.getOrElse(right,Set[HeapIdentifier]())
              case right: FieldIdentifier => objFieldToObj.getOrElse(right.obj,Map[String,Set[HeapIdentifier]]()).
                getOrElse(right.field,Set[HeapIdentifier]())
              case _ => Set[HeapIdentifier]()
            }
            val intersection = l intersect r
            val lr = l diff intersection
            val rl = r diff intersection
            if (lr.isEmpty && rl.isEmpty && intersection.size == 1 && intersection.head.representsSingleVariable) {
              // there are only common Obj
              this.bottom() // return the bottom state
            } else { // there is at least a common Obj
              var refToObjmap = refToObj
              refToObjmap = left match {
                case left: VariableIdentifier => refToObjmap + (left -> lr)
                case _ => refToObjmap
              }
              refToObjmap = right match {
                case right: VariableIdentifier => refToObjmap + (right -> rl)
                case _ => refToObjmap
              }
              var objFieldToObjmap = objFieldToObj
              objFieldToObjmap = left match {
                case left: FieldIdentifier =>
                  objFieldToObjmap + (left.obj -> Map[String,Set[HeapIdentifier]](left.field -> lr))
                case _ => objFieldToObjmap
              }
              objFieldToObjmap = right match {
                case right: FieldIdentifier =>
                  objFieldToObjmap + (right.obj -> Map[String,Set[HeapIdentifier]](right.field -> rl))
                case _ => objFieldToObjmap
              }
              // return the current state with updated refToObj and objFieldToObj
              this.copy(refToObj = refToObjmap, objFieldToObj = objFieldToObjmap).pruneUnreachableHeap()
            }
          case (left: Identifier, Constant("null",_,_)) =>
            val l = left match {
              case left: VariableIdentifier => refToObj.getOrElse(left,Set[HeapIdentifier]())
              case left: FieldIdentifier => objFieldToObj.getOrElse(left.obj,Map[String,Set[HeapIdentifier]]()).
                getOrElse(left.field,Set[HeapIdentifier]())
              case _ => Set[HeapIdentifier]()
            }
            val r = Set[HeapIdentifier](NullHeapIdentifier)
            val intersection = l intersect r
            val lr = l diff intersection
            if (lr.isEmpty && intersection.size == 1 && intersection.head.representsSingleVariable)
              this.bottom()
            else {
              // replace key into refToObj map
              val refToObjmap = left match {
                case left: VariableIdentifier => refToObj + (left -> lr)
                case _ => refToObj
              }
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap).pruneUnreachableHeap()
            }
          case (Constant("null",_,_), right: Identifier) =>
            val l = Set[HeapIdentifier](NullHeapIdentifier)
            val r = right match {
              case right: VariableIdentifier => refToObj.getOrElse(right,Set[HeapIdentifier]())
              case right: FieldIdentifier => objFieldToObj.getOrElse(right.obj,Map[String,Set[HeapIdentifier]]()).
                getOrElse(right.field,Set[HeapIdentifier]())
              case _ => Set[HeapIdentifier]()
            }
            val intersection = l intersect r
            val rl = r diff intersection
            if (rl.isEmpty && intersection.size == 1 && intersection.head.representsSingleVariable)
              this.bottom()
            else {
              // replace key into refToObj map
              val refToObjmap = right match {
                case right: VariableIdentifier => refToObj + (right -> rl)
                case _ => refToObj
              }
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap).pruneUnreachableHeap()
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
  override def before(pp: ProgramPoint): S = {
    logger.debug("\n*** ----------------before(" + pp.toString + "): " + this.repr)
    // return the current state with updated currentPP
    this.copy(currentPP = pp)
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): S = {
    // logger.debug("*** bottom()")

    // return a new state with bottom exprSet, empty refToObj, empty objFieldToObj, bottom numDom
    val expr = exprSet.bottom()
    val fields = Set[(Type,String)]()
    val refToObjmap = Map[VariableIdentifier,Set[HeapIdentifier]]()
    val objFieldToObjmap = Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]]()
    val num = numDom.bottom()
    val currentPP = DummyProgramPoint
    this.copy(expr,fields,refToObjmap,objFieldToObjmap,num,currentPP)
  }

  def copy(exprSet: ExpressionSet = exprSet,
           fieldSet: Set[(Type, String)] = fieldSet,
           refToObj: Map[VariableIdentifier, Set[HeapIdentifier]] = refToObj,
           objFieldToObj: Map[HeapIdentifier, Map[String, Set[HeapIdentifier]]] = objFieldToObj,
           numDom: T = numDom,
           currentPP: ProgramPoint = currentPP): S

  /** Creates an object at allocation site.
    *
    * Invoked by calls to `new()`.
    *
    * @param typ The dynamic type of the created object
    * @param pp The allocation site of the object
    * @return The abstract state with updated exprSet after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): S = {
    logger.debug("*** ----------------createObject(" + typ.toString + "; " + pp.toString + ")")
    
    val obj = HeapIdentifier(typ, pp) // create new Obj
    if (objFieldToObj.contains(obj)) { // the Obj was created already
      obj.setSummary(true)  // turn the Obj into a summary node
      // update refToObj map adding the summary node version of Obj where Obj was present
      val refToObjmap = refToObj.mapValues(s => if (s.contains(obj)) s - obj + obj else s)
      // add key to objFieldToObj map
      var objFieldToObjmap = objFieldToObj - obj + (obj -> objFieldToObj(obj))
      // update objFieldToObj map adding the summary node version of Obj where Obj was present
      objFieldToObjmap = objFieldToObjmap.mapValues(
       m => m.mapValues(s => if (s.contains(obj)) s - obj + obj else s)
      )
      // return the current state with updated exprSet, update refToObj and updated objFieldToObj map
      this.copy(exprSet = ExpressionSet(obj), refToObj = refToObjmap, objFieldToObj = objFieldToObjmap)
    } else { // the Obj was never created before
      // prepare fields to add to objFieldToObj map and add variables to numDom
      var fieldMap = Map[String,Set[HeapIdentifier]]()
      var num = numDom
      for (f <- fieldSet) {
        f._1 match {
          case _:RefType => fieldMap = fieldMap + (f._2 -> Set[HeapIdentifier]())
          case _ => num = num.createVariable(FieldIdentifier(obj,f._2,f._1),f._1)
        }
      }
      // add key to objFieldToObj map
      val objFieldToObjmap = objFieldToObj + (obj -> fieldMap)
      // return the current state with updated exprSet, updated objFieldToObj map and updated numDom
      this.copy(exprSet = ExpressionSet(obj), objFieldToObj = objFieldToObjmap, numDom = num)
    }
  }

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    * Invoked by variable declarations (`var x : Ref`, ...)
    *
    * @param x The name of the variable
    * @param typ The static type of the variable
    * @param pp The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): S = {
    logger.debug("*** ----------------createVariable(" + x.toString + "; " + typ.toString + "; " + pp.toString + ")")

    if (typ.isObject) { // the variable to be created is a `Ref`
      // add key to refToObj map
      val refToObjmap = refToObj + (x -> Set[HeapIdentifier](NullHeapIdentifier))
      // return the current state with updated exprSet and refToObj
      this.copy(exprSet = ExpressionSet(x), refToObj = refToObjmap)
    } else { // the variable to be created is not a `Ref`
      // return the current state with updated numDom
      this.copy(numDom = numDom.createVariable(x,typ))
    }
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): S = {
    logger.debug("*** ----------------createVariableForArgument(" + x.toString + "; " + typ.toString + ")")

    if (typ.isObject) { // the variable to be created is a `Ref`
      val obj = HeapIdentifier(typ, x.pp) // create new Obj
      if (objFieldToObj.contains(obj)) { // the Obj was created already
        obj.setSummary(true)  // turn the Obj into a summary node
        // update refToObj map adding the summary node version of Obj where Obj was present
        var refToObjmap = refToObj.mapValues(s => if (s.contains(obj)) s - obj + obj else s)
        // add key to refToObj map
        refToObjmap = refToObjmap + (x -> Set[HeapIdentifier](NullHeapIdentifier, obj))
        // add key to objFieldToObj map
        var objFieldToObjmap = objFieldToObj - obj + (obj -> objFieldToObj(obj))
        // update objFieldToObj map adding the summary node version of Obj where Obj was present
        objFieldToObjmap = objFieldToObjmap.mapValues(
          m => m.mapValues(s => if (s.contains(obj)) s - obj + obj else s)
        )
        // return the current state with updated exprSet, update refToObj and updated objFieldToObj map
        this.copy(exprSet = ExpressionSet(x), refToObj = refToObjmap, objFieldToObj = objFieldToObjmap)
      } else { // the Obj was never created before
        // prepare fields to add to objFieldToObj map and add variables to numDom
        val sumPP: WrappedProgramPoint = x.pp match {
          case pp:WrappedProgramPoint =>
            WrappedProgramPoint(LineColumnPosition(line = pp.pos.line,column = -1))
          case _ => WrappedProgramPoint(LineColumnPosition(line = -1,column = -1))
        }
        val sumObj = HeapIdentifier(typ, sumPP).setSummary(true)
        var objFieldMap = Map[String,Set[HeapIdentifier]]()
        var sumFieldMap = Map[String,Set[HeapIdentifier]]()
        var num = numDom
        for (f <- fieldSet) {
          f._1 match {
            case _:RefType =>
              objFieldMap = objFieldMap + (f._2 -> Set[HeapIdentifier](obj, sumObj, NullHeapIdentifier))
              sumFieldMap = sumFieldMap + (f._2 -> Set[HeapIdentifier](sumObj, NullHeapIdentifier))
            case _ => num = num.createVariable(FieldIdentifier(obj,f._2,f._1),f._1).
              createVariable(FieldIdentifier(sumObj,f._2,f._1),f._1)
          }
        }
        // add key to refToObj map
        val refToObjmap = refToObj + (x -> Set[HeapIdentifier](NullHeapIdentifier, obj))
        // add key to objFieldToObj map
        val objFieldToObjmap = objFieldToObj + (obj -> objFieldMap) + (sumObj -> sumFieldMap)
        // return the current state with updated exprSet, refToObj, objFieldToObj, numDom
        this.copy(exprSet = ExpressionSet(x), refToObj = refToObjmap, objFieldToObj = objFieldToObjmap, numDom = num)
      }
    } else { // the variable to be created is not a `Ref`
      // return the current state with updated numDom
      this.copy(numDom = numDom.createVariable(x,typ))
    }
  }

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ The type of the numerical constant
    * @param pp The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): S = {
    // logger.debug("*** evalConstant(" + value + "; " + typ.toString + "; " + pp.toString + ")")

    val const = new Constant(value, typ, pp)
    // return the current state with updated exprSet
    this.copy(exprSet = ExpressionSet(const))
  }

  /** Evaluates a path of object fields
    *
    * @param path the object fields path to evaluate
    * @param objFieldToObjmap the current objFieldToObj map updated with missing fields
    * @return the set of objects referenced by the path (except the last field)
    */
  def evaluatePath(path: List[String],
                   objFieldToObjmap: Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]]) : Set[HeapIdentifier] = {
    val keys = refToObj.keySet // set of all Ref variables
    // retrieving the Ref variable corresponding to the head of the path
    val id = keys.find((ref) => ref.name == path.head).get
    val fst = refToObj(id) // set of objects pointed by the Ref variable
    // path evaluation
    val ids = path.drop(1).dropRight(1).foldLeft(fst)(
      (set,next) => // next path segment
        set.foldLeft(Set[HeapIdentifier]())(
          (s,obj) => s ++ objFieldToObjmap.getOrElse(obj,Map[String,Set[HeapIdentifier]]()).
            getOrElse(next,Set[HeapIdentifier]())
        )
    )
    if (ids.contains(NullHeapIdentifier)) Reporter.reportGenericWarning("Possible null pointer dereference", currentPP)
    ids - NullHeapIdentifier // return the objects referenced by the path (except the last field)
  }

  /** The current expression.
    *
    * Invoked after each statement to retrieve its result.
    */
  override def expr: ExpressionSet = {
    // logger.debug("*** expr: " + this.exprSet.toString)
    
    this.exprSet // return exprSet
  }

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): S = {
    // logger.debug("*** factory()")

    val expr = ExpressionSet()
    val fields = Set[(Type,String)]()
    val refToObjmap = Map[VariableIdentifier,Set[HeapIdentifier]]()
    val objFieldToObjmap = Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]]()
    val num = numDom.factory()
    val currentPP = DummyProgramPoint
    this.copy(expr,fields,refToObjmap,objFieldToObjmap,num,currentPP)
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object from which the field access is performed
    * @param field the name of the field to be accessed
    * @param typ the type of the field to be accessed
    * @return The abstract state obtained after the field access, that is, a new state whose `ExpressionSet`
    *         holds the objects referenced by the access path (up to the given field excluded).
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): S = {
    logger.debug("*** ----------------getFieldValue(" + obj.toString + "; " + field + "; " + typ.toString + ")")

    obj match {
      case obj:AccessPathIdentifier =>
        val path = obj.stringPath // path to evaluate
        // evaluate path into the set of objects referenced by it (up to the given field excluded)
        val objSet = evaluatePath(path,objFieldToObj) - NullHeapIdentifier
        val expr = objSet.foldLeft(ExpressionSet())(
          (e,o) => {
            val fld = FieldIdentifier(o,field,typ) // create new FieldIdentifier
            e add ExpressionSet(fld)
          }
        )
        // return the current state with updated exprSet
        this.copy(exprSet = expr)
      case _ => throw new IllegalArgumentException("A field access must occur via an AccessPathIdentifier")
    }
  }

  /** Gets the value of a variable.
    *
    * Invoked by variable declarations (`var x : Ref`) and variable assignments (`x = ...`, `... = x`)
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): S = {
    logger.debug("*** ----------------getVariableValue(" + id.toString + ")")

    // return the current state with updated exprSet
    this.copy(exprSet = ExpressionSet(id))
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    * @todo implement me!
    */
  override def glb(other: S): S = {
    logger.debug("*** glb(" + other.repr + "): implement me!")
    
    ???
  }

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return `true` if and only if the state is equivalent to bottom
    */
  override def isBottom: Boolean = {
    // logger.debug("*** isBottom: " + this.repr)

    numDom.isBottom
  }

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return `true` if and only if the state is equivalent to top
    */
  override def isTop: Boolean = {
    // logger.debug("*** isTop")
    
    numDom.isTop
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return `true` if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: S): Boolean = {
    logger.debug("*** lessEqual(" + this.repr + ", " + other.repr + ")")

    //val exp = this.exprSet lessEqual other.exprSet // test the exprSets
    //val fields = this.fieldSet subsetOf other.fieldSet // test the fieldSets
    val refToObjmap = this.refToObj.forall {
      case (k: VariableIdentifier,v: Set[HeapIdentifier]) => v subsetOf other.refToObj.getOrElse(k,Set[HeapIdentifier]())
    } // test the refToObjs
    val objFieldToObjmap = this.objFieldToObj.forall {
      case (o: HeapIdentifier, m: Map[String,Set[HeapIdentifier]]) => m.forall {
        case (f: String, s: Set[HeapIdentifier]) =>
          s subsetOf other.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()).getOrElse(f,Set[HeapIdentifier]())
      }
    } // test the objFieldToObjs
    val num = this.numDom.lessEqual(other.numDom) // test the numDoms
    refToObjmap && objFieldToObjmap && num
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: S): S = {
    logger.debug("*** lub(" + this.repr + ", " + other.repr + ")")

    val exp = this.exprSet lub other.expr // join the exprSets
    val fields = this.fieldSet ++ other.fieldSet // join the fieldSets
    val refToObjmap = this.refToObj.filterKeys(k => !other.refToObj.contains(k)) ++ other.refToObj.map {
      case (k: VariableIdentifier,v: Set[HeapIdentifier]) => k -> (v ++ this.refToObj.getOrElse(k,Set[HeapIdentifier]()))
    } // merge the refToObjs
    def mergeFieldMaps (first: Map[String,Set[HeapIdentifier]], second: Map[String,Set[HeapIdentifier]]) = {
      first.filterKeys(k => !second.contains(k)) ++ second.map {
        case (k: String, v: Set[HeapIdentifier]) => k -> (v ++ first.getOrElse(k,Set[HeapIdentifier]()))
      }
    } // method to merge field maps
    val objFieldToObjmap = this.objFieldToObj.filterKeys(k => !other.objFieldToObj.contains(k)) ++ other.objFieldToObj.map {
      case (o: HeapIdentifier,m: Map[String,Set[HeapIdentifier]]) => o ->
        mergeFieldMaps(m, this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()))
    } // merge the objFieldToObjmap
    val num = this.numDom lub other.numDom // join the numDoms

    // return the current state with updated exprSet, fieldSet, refToObj, objFieldToObj and numDom
    this.copy(exprSet = exp, fieldSet = fields, refToObj = refToObjmap, objFieldToObj = objFieldToObjmap, numDom = num)
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): S = {
    // logger.debug("*** pruneUnreachableHeap()")

    // retrieve all Obj reachable from Ref variables
    val objFromRef = refToObj.foldLeft(Set[HeapIdentifier]())((r, s) => r ++ s._2)
    // add all Obj reachable from fields of Obj reachable from Ref variables
    var reach = objFromRef
    for (id <- objFromRef) {
      reach = objFieldToObj.getOrElse(id,Map[String,Set[HeapIdentifier]]()).foldLeft(reach)((r,s) => r ++ s._2)
    }
    // collect and remove all unreachable Obj
    var unreach = Set[HeapIdentifier]()
    var objFieldToObjmap = objFieldToObj
    for (key <- objFieldToObj.keys) {
      if (!reach.contains(key)) {
        objFieldToObjmap = objFieldToObjmap - key
        unreach = unreach + key
      }
    }
    // prune variables from numDom
    var num = numDom
    for (obj <- unreach) {
      for (f <- fieldSet) {
        num = num.removeVariable(FieldIdentifier(obj,f._2,f._1))
      }
    }

    // return the current state with updated objFieldToObj and numDom
    this.copy(objFieldToObj = objFieldToObjmap, numDom = num)
  }

  /** Removes all variables satisfying filter.
    *
    * @todo implement me!
    */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): S = {
    logger.debug("*** pruneVariables(" + filter.toString + "): implement me!")
    
    ???
  }

  /** Removes the current expression.
    *
    * @return The abstract state obtained after removing the current expression
    */
  override def removeExpression(): S = {
    // logger.debug("*** removeExpression()")

    // return the current state with a new exprSet
    this.copy(exprSet = ExpressionSet())
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    * @todo implement me!
    */
  override def removeVariable(varExpr: VariableIdentifier): S = {
    logger.debug("*** removeVariable(" + varExpr.toString + "): implement me!")
    
    ???
  }

  /** The default state string representation.
    *
    * @return the default string representation of the current state
    */
  def repr: String = {
    "PointsToNumericalState(" +
      exprSet.toString + ", " +
      refToObj.toString + ", " +
      objFieldToObj.toString + ", " +
      numDom.toString + ")"
  }

  /** Assigns an expression to an argument.
    *
    * @param x The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    * @todo implement me!
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): S = {
    logger.debug("*** setArgument(" + x.toString + "; " + right.toString + "): implement me!")
    
    ???
  }

  /** Sets the current expression.
    *
    * Invoked after statements that do not have results (like assignments to variables and fields).
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): S = {
    // logger.debug("*** setExpression(" + expr.toString + ")")
    this.copy(exprSet = expr) // return the current state with updated exprSet
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    * @todo implement me!
    */
  override def setVariableToTop(varExpr: Expression): S = {
    logger.debug("*** setVariableToTop(" + varExpr.toString + "): implement me!")
    
    ???
  }

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    * @todo implement me!
    */
  override def throws(t: ExpressionSet): S = {
    logger.debug("*** throws(" + t.toString + "): implement me!")
    
    ???
  }

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): S = {
    // logger.debug("*** top()")

    // return a new state with top exprSet, empty refToObj, empty objFieldToObj, top numDom
    val expr = exprSet.top()
    val fields = Set[(Type,String)]()
    val refToObjmap = Map[VariableIdentifier,Set[HeapIdentifier]]()
    val objFieldToObjmap = Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]]()
    val num = numDom.top()
    val currentPP = DummyProgramPoint
    this.copy(expr,fields,refToObjmap,objFieldToObjmap,num,currentPP)
  }

  /** The state string representation.
    *
    * @return the string representation of the current state
    */
  override def toString: String = {
    "PointsToNumericalState(\n" +
    "\texprSet: " + exprSet.toString + "\n" +
    //"\tfieldSet: " + fieldSet.toString + "\n" +
    "\trefToObj: " + refToObj.toString + "\n" +
    "\tobjFieldToObj: " + objFieldToObj.toString + "\n" +
    "\tnumDom: " + numDom.toString + "\n" +
    ")"
  }

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: S): S = {
    logger.debug("*** ----------------widening(" + other.repr + ")")

    val exp = this.exprSet widening other.expr // widen the exprSets
    val fields = this.fieldSet ++ other.fieldSet // join the fieldSets
    val refToObjmap = this.refToObj.filterKeys(k => !other.refToObj.contains(k)) ++ other.refToObj.map {
      case (k: VariableIdentifier,v: Set[HeapIdentifier]) => k -> (v ++ this.refToObj.getOrElse(k,Set[HeapIdentifier]()))
    } // merge the refToObjs
    def mergeFieldMaps (first: Map[String,Set[HeapIdentifier]], second: Map[String,Set[HeapIdentifier]]) = {
      first.filterKeys(k => !second.contains(k)) ++ second.map {
        case (k: String, v: Set[HeapIdentifier]) => k -> (v ++ first.getOrElse(k,Set[HeapIdentifier]()))
      }
    } // method to merge field maps
    val objFieldToObjmap = this.objFieldToObj.filterKeys(k => !other.objFieldToObj.contains(k)) ++ other.objFieldToObj.map {
        case (o: HeapIdentifier,m: Map[String,Set[HeapIdentifier]]) => o ->
          mergeFieldMaps(m, this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()))
    } // merge the objFieldToObjmap
    val num = this.numDom widening other.numDom // widen the numDoms

    // return the current state with updated exprSet, fieldSet, refToObj, objFieldToObj and numDom
    this.copy(exprSet = exp, fieldSet = fields, refToObj = refToObjmap, objFieldToObj = objFieldToObjmap, numDom = num)
  }
}

/** PointsTo+Intervals Analysis State.
  *
  * @param exprSet `ExpressionSet` used to store the result of each statement
  * @param fieldSet fields declared within the program
  * @param refToObj map from `Ref` variables to heap objects
  * @param objFieldToObj map from heap objects to a map from `Ref` fields to heap objects
  * @param numDom intervals abstract domain
  * @author Caterina Urban
  */
case class PointsToIntervalsState(exprSet: ExpressionSet,
                                  fieldSet: Set[(Type, String)],
                                  refToObj: Map[VariableIdentifier, Set[HeapIdentifier]],
                                  objFieldToObj: Map[HeapIdentifier, Map[String, Set[HeapIdentifier]]],
                                  numDom: BoxedNonRelationalNumericalDomain[DoubleInterval],
                                  currentPP: ProgramPoint)
  extends PointsToNumericalState[BoxedNonRelationalNumericalDomain[DoubleInterval],PointsToIntervalsState] {
  override def copy(exprSet: ExpressionSet,
                    fieldSet: Set[(Type, String)],
                    refToObj: Map[VariableIdentifier, Set[HeapIdentifier]],
                    objFieldToObj: Map[HeapIdentifier, Map[String, Set[HeapIdentifier]]],
                    numDom: BoxedNonRelationalNumericalDomain[DoubleInterval],
                    currentPP: ProgramPoint): PointsToIntervalsState =
    PointsToIntervalsState(exprSet, fieldSet, refToObj, objFieldToObj, numDom, currentPP)
}

/** PointsTo+Polyhedra Analysis State.
  *
  * @param exprSet `ExpressionSet` used to store the result of each statement
  * @param fieldSet fields declared within the program
  * @param refToObj map from `Ref` variables to heap objects
  * @param objFieldToObj map from heap objects to a map from `Ref` fields to heap objects
  * @param numDom polyhedra abstract domain
  * @author Caterina Urban
  */
case class PointsToPolyhedraState(exprSet: ExpressionSet,
                                  fieldSet: Set[(Type, String)],
                                  refToObj: Map[VariableIdentifier, Set[HeapIdentifier]],
                                  objFieldToObj: Map[HeapIdentifier, Map[String, Set[HeapIdentifier]]],
                                  numDom: Apron.Polyhedra,
                                  currentPP: ProgramPoint)
  extends PointsToNumericalState[Apron.Polyhedra,PointsToPolyhedraState] {
  override def copy(exprSet: ExpressionSet,
                    fieldSet: Set[(Type, String)],
                    refToObj: Map[VariableIdentifier, Set[HeapIdentifier]],
                    objFieldToObj: Map[HeapIdentifier, Map[String, Set[HeapIdentifier]]],
                    numDom: Polyhedra,
                    currentPP: ProgramPoint): PointsToPolyhedraState =
    PointsToPolyhedraState(exprSet, fieldSet, refToObj, objFieldToObj, numDom, currentPP)
}

/** PointsTo+Numerical analysis entry states for given method declarations.
  *
  * @tparam T the numerical domain
  * @tparam S the pointsto+numerical state
  * @author Caterina Urban
  */
trait PointsToNumericalEntryStateBuilder[T <: NumericalDomain[T], S <: PointsToNumericalState[T,S]] extends ForwardEntryStateBuilder[S] {

  protected var fields: Set[(Type,String)] = Set[(Type,String)]()

  override def build(method: MethodDeclaration): S = {
    fields = Set[(Type,String)]()
    for(f <- method.classDef.fields) {
      fields = fields + ((f.typ, f.variable.toString))
    }
    method.initializeArgument[S](topState.copy(fieldSet = fields))
  }

}

/** PointsTo+Intervals analysis entry states for given method declarations.
  *
  * @author Caterina Urban
  */
object PointsToIntervalsEntryStateBuilder
  extends PointsToNumericalEntryStateBuilder[BoxedNonRelationalNumericalDomain[DoubleInterval], PointsToIntervalsState] {

  override def topState = PointsToIntervalsState(ExpressionSet(), fields,
    Map[VariableIdentifier,Set[HeapIdentifier]](),
    Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]](),
    new BoxedNonRelationalNumericalDomain[DoubleInterval](DoubleInterval.Top), DummyProgramPoint)

}

/** PointsTo+Polyhedra analysis entry states for given method declarations.
  *
  * @author Caterina Urban
  */
object PointsToPolyhedraEntryStateBuilder
  extends PointsToNumericalEntryStateBuilder[Apron.Polyhedra, PointsToPolyhedraState] {

  override def topState = PointsToPolyhedraState(ExpressionSet(), fields,
    Map[VariableIdentifier,Set[HeapIdentifier]](),
    Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]](),
    Apron.Polyhedra.Bottom.factory(), DummyProgramPoint)

}

/** Runs the PointsTo+Numerical analysis.
  *
  * @tparam N the numerical domain
  * @tparam T the state
  * @author Caterina Urban
  */
trait PointsToNumericalAnalysisRunner[N <: NumericalDomain[N], T <: PointsToNumericalState[N,T]] extends SilverAnalysisRunner[T] {

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
          val states: List[T] = g.blockStates(i).last // post-states of each statement
          for (s <- states) {
            println("\n******************* \n")
            println(s)
          }
        } else {
          // printing the block pre-state
          println("\n+++++++++++++++++++ BLOCK " + i + "+++++++++++++++++++\n")
          println(g.blockStates(i).last.head)
          val states: List[T] = g.blockStates(i).last.drop(1) // post-states of each statement
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

  override def toString = "PointsTo+Numerical Analysis"
}

/** Runs the PointsTo+Intervals analysis.
  *
  * @author Caterina Urban
  */
object PointsToIntervalsAnalysisRunner
  extends PointsToNumericalAnalysisRunner[BoxedNonRelationalNumericalDomain[DoubleInterval], PointsToIntervalsState] {
  override val analysis = SimpleForwardAnalysis[PointsToIntervalsState](PointsToIntervalsEntryStateBuilder)
  override def toString = "PointsTo+Intervals Analysis"
}

/** Runs the PointsTo+Polyhedra analysis.
  *
  * @author Caterina Urban
  */
object PointsToPolyhedraAnalysisRunner
  extends PointsToNumericalAnalysisRunner[Apron.Polyhedra, PointsToPolyhedraState] {
  override val analysis = SimpleForwardAnalysis[PointsToPolyhedraState](PointsToPolyhedraEntryStateBuilder)
  override def toString = "PointsTo+Polyhedra Analysis"
}
