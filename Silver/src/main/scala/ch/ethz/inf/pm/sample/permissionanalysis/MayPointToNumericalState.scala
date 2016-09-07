/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.execution.{ForwardEntryStateBuilder, SimpleForwardAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

/** Heap node.
  *
  * @param number the unique identifier of the heap node
  * @author Caterina Urban
  */
case class OldHeapNode(number: Int) extends Identifier.HeapIdentifier
{
  override def getName: String = "0" + number
  override def equals(o: Any) = o match {
    case that: OldHeapNode => this.getName equals that.getName
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
object SummaryOldHeapNode$ extends OldHeapNode(0) {
  override def representsSingleVariable: Boolean = false
  override def toString: String = "Σ"
}

/** Null heap node.
  *
  * @author Caterina Urban
  */
object NullOldHeapNode$ extends OldHeapNode(-1) {
  override def getName: String = "null"
  override def representsSingleVariable: Boolean = false
  override def toString: String = "null"
}

/** Field access.
  *
  * @param obj the receiver heap node
  * @param field the field name
  * @param typ the field type
  * @author Caterina Urban
  */
case class HeapAccess(obj: OldHeapNode, field: String, typ: Type) extends Identifier.FieldIdentifier {
  override def getName: String = obj.getName + "." + field
  override def getField: Option[String] = Some(field)
  override def pp: ProgramPoint = obj.pp
  override def representsSingleVariable: Boolean = obj.representsSingleVariable
}

/** MayPointTo+Numerical Analysis State.
  *
  * The may-point-to analysis is an allocation-site abstraction with materialization.
  * The numerical analysis uses a numerical abstract domain.
  *
  * @tparam T the numerical domain
  * @tparam S the maypointto+numerical state
  * @author Caterina Urban
  */
trait MayPointToNumericalState[T <: NumericalDomain[T], S <: MayPointToNumericalState[T,S]]
  extends SimpleState[S] with StateWithRefiningAnalysisStubs[S] with SilverSpecification with LazyLogging
{
  this: S =>

  def fieldSet: Set[(Type,String)] // fields declared within the program

  def currentPP: ProgramPoint // current program point
  def flag: Boolean // true = materialization allowed; false = materialization not allowed
  def nonce: Int // fresh number for newly created heap nodes

  def exprSet: ExpressionSet // result of previous statement
  // map from Ref variables to heap objects
  def refToObj: Map[VariableIdentifier,Set[OldHeapNode]]
  // map from heap objects to a map from Ref fields to heap objects
  def objToObj: Map[OldHeapNode,Map[String,Set[OldHeapNode]]]
  def numDom: T // numerical abstract domain

  /** Generates a Silver invariant from the current state
    *
    * @return a sequence of sil.Exp
    */
  override def invariant(): Seq[sil.Exp] = Seq[sil.Exp]() //TODO:

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
    logger.trace("*** ----------------assignField(" + obj.toString + "; " + field.toString + "; " + right.toString + ")")

    obj match {
      case obj: HeapAccess =>
        if (obj.typ.isObject) { // the assigned field is a Ref
          right match {
            case right: HeapAccess => // e.g., `x.f := y.g`
              val s: Set[OldHeapNode] = objToObj(right.obj)(right.field) // retrieve the heap Obj objects
              val o: OldHeapNode = obj.obj // retrieve `Obj` whose field is assigned
              val f: String = obj.field // retrieve assigned field
              val objMap = if (o.representsSingleVariable) { // strong update
                objToObj + (o -> (objToObj(o) + (f -> s)))
              } else { // weak update
                objToObj + (o -> (objToObj(o) + (f -> (objToObj(o)(f) ++ s))))
              }
              // return the current state with updated objToObj
              this.copy(objToObj = objMap).pruneUnreachableHeap()

            case right: Constant => // e.g., `x.f := null`
              val o = obj.obj // retrieve `Obj` whose field is assigned
              val f = obj.field // retrieve assigned field
              // weak update
              val objMap = if (o.representsSingleVariable) { // strong update
                objToObj + (o -> (objToObj(o) + (f -> Set[OldHeapNode](NullOldHeapNode$))))
              } else { // weak update
                objToObj + (o -> (objToObj(o) + (f -> (objToObj(o)(f) ++ Set[OldHeapNode](NullOldHeapNode$)))))
              }
              // return the current state with updated objToObj
              this.copy(objToObj = objMap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x.f := y`
              val s = refToObj(right) // retrieve the corresponding heap `Obj` objects
              val o = obj.obj // retrieve `Obj` whose field is assigned
              val f = obj.field // retrieve assigned field
              // weak update
              val objMap = if (o.representsSingleVariable) { // strong update
                objToObj + (o -> (objToObj(o) + (f -> s)))
              } else { // weak update
                objToObj + (o -> (objToObj(o) + (f -> (objToObj(o)(f) ++ s))))
              }
              // return the current state with updated objToObj
              this.copy(objToObj = objMap).pruneUnreachableHeap()

            case _ => throw new NotImplementedError("A field assignment implementation is missing.")
          }
        } else {  // the assigned field is not a Ref
          val num = if (obj.obj.representsSingleVariable) { // strong update
            numDom.assign(obj,right)
          } else { // weak update
            numDom lub numDom.assign(obj,right)
          }
          // return the current state with updated numDom
          this.copy(numDom = num)
        }
      case _ => throw new IllegalArgumentException("A field assignment must occur via a HeapAccess.")
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
    logger.trace("*** ----------------assignVariable(" + x.toString + "; " + right.toString + ")")

    x match {
      case x: VariableIdentifier =>
        if (x.typ.isObject) { // the assigned variable is a Ref
          right match {
            case right: HeapAccess => // e.g., `x := y.g`
              val s = this.objToObj(right.obj)(right.field) // retrieve the heap `Obj` objects
              // add xref -> s to refToObj map
              val refMap = this.refToObj + (x -> s)
              // return the current state with updated refToObj
              this.copy(refToObj = refMap).pruneUnreachableHeap()

            case right: OldHeapNode => // e.g., `x = new()`
              // add xref -> right to refToObj map
              val refMap = this.refToObj + (x -> Set[OldHeapNode](right))
              // return the current state with updated refToObj
              this.copy(refToObj = refMap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x := y`
              // add xref -> refToObj[rightref] to refToObj map
              val refMap = this.refToObj + (x -> this.refToObj.getOrElse(right, Set[OldHeapNode](NullOldHeapNode$)))
              // return the current state with updated refToObj
              this.copy(refToObj = refMap).pruneUnreachableHeap()

            case _ => throw new NotImplementedError("A variable assignment implementation is missing.")
          }
        } else {  // the assigned variable is not a Ref
          this.copy(numDom = numDom.assign(x,right))  // return the current state with updated numDom
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
    logger.trace("*** ----------------assume(" + cond.toString + ")")

    cond match {
      case cond:Constant => // Constant
        val num = numDom.assume(cond)
        if (num.isBottom) this.bottom() else this.copy(numDom = num)

      case cond: Identifier => // Identifier
        val num = numDom.assume(cond)
        if (num.isBottom) this.bottom() else this.copy(numDom = num)

      case cond: BinaryArithmeticExpression => // BinaryArithmeticExpression
        val num = numDom.assume(cond)
        if (num.isBottom) this.bottom() else this.copy(numDom = num)

      case BinaryBooleanExpression(left, right, BooleanOperator.&&, typ) => // BinaryBooleanExpression
        if (cond.canonical) {
          val num = numDom.assume(cond)
          if (num.isBottom) this.bottom() else this.copy(numDom = num)
        } else this.assume(left).assume(right)
      case BinaryBooleanExpression(left, right, BooleanOperator.||, typ) => // BinaryBooleanExpression
        if (cond.canonical) {
          val num = numDom.assume(cond)
          if (num.isBottom) this.bottom() else this.copy(numDom = num)
        } else this.assume(left) lub this.assume(right)

      case cond: NegatedBooleanExpression => { // NegatedBooleanExpression
        cond.exp match {
          case c: Constant => // Constant
            val num = numDom.assume(cond)
            if (num.isBottom) this.bottom() else this.copy(numDom = num)

          case id: Identifier => // Identifier
            val num = numDom.assume(cond)
            if (num.isBottom) this.bottom() else this.copy(numDom = num)

          case BinaryArithmeticExpression(left, right, op, typ) => // BinaryArithmeticExpression
            this.assume(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op), typ))

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
      }

      case ReferenceComparisonExpression(left, right, ArithmeticOperator.==, typ) =>
        (left, right) match {
          case (left: Identifier, right: Identifier) =>
            val l = left match {
              case left: VariableIdentifier => refToObj.getOrElse(left,Set[OldHeapNode]())
              case left: HeapAccess => objToObj.getOrElse(left.obj,Map[String,Set[OldHeapNode]]()).
                getOrElse(left.field,Set[OldHeapNode]())
              case _ => Set[OldHeapNode]()
            }
            val r = right match {
              case right: VariableIdentifier => refToObj.getOrElse(right,Set[OldHeapNode]())
              case right: HeapAccess => objToObj.getOrElse(right.obj,Map[String,Set[OldHeapNode]]()).
                getOrElse(right.field,Set[OldHeapNode]())
              case _ => Set[OldHeapNode]()
            }
            val intersection = l intersect r
            if (intersection.isEmpty) { // there is no common heap node
              this.bottom() // return the bottom state
            } else { // there is at least one common heap node
              var refMap = refToObj
              refMap = left match {
                case left: VariableIdentifier => refMap + (left -> intersection)
                case _ => refMap
              }
              refMap = right match {
                case right: VariableIdentifier => refMap + (right -> intersection)
                case _ => refMap
              }
              var objMap = objToObj
              objMap = left match {
                case left: HeapAccess => objMap + (left.obj -> Map[String,Set[OldHeapNode]](left.field -> intersection))
                case _ => objMap
              }
              objMap = right match {
                case right: HeapAccess => objMap + (right.obj -> Map[String,Set[OldHeapNode]](right.field -> intersection))
                case _ => objMap
              }
              // return the current state with updated refToObj and objToObj
              this.copy(refToObj = refMap, objToObj = objMap).pruneUnreachableHeap()
            }
          case (left: Identifier, Constant("null",_,_)) =>
            val l = left match {
              case left: VariableIdentifier => refToObj.getOrElse(left,Set[OldHeapNode]())
              case left: HeapAccess => objToObj.getOrElse(left.obj,Map[String,Set[OldHeapNode]]()).
                getOrElse(left.field,Set[OldHeapNode]())
              case _ => Set[OldHeapNode]()
            }
            if (l.contains(NullOldHeapNode$)) {
              val refMap = left match {
                case left: VariableIdentifier => refToObj + (left -> Set[OldHeapNode](NullOldHeapNode$))
                case _ => refToObj
              }
              this.copy(refToObj = refMap).pruneUnreachableHeap() // return the current state with updated refToObj
            } else this.bottom() // return the bottom state
          case (Constant("null",_,_), right: Identifier) =>
            val r = right match {
              case right: VariableIdentifier => refToObj.getOrElse(right,Set[OldHeapNode]())
              case right: HeapAccess => objToObj.getOrElse(right.obj,Map[String,Set[OldHeapNode]]()).
                getOrElse(right.field,Set[OldHeapNode]())
              case _ => Set[OldHeapNode]()
            }
            if (r.contains(NullOldHeapNode$)) {
              val refMap = right match {
                case right: VariableIdentifier => refToObj + (right -> Set[OldHeapNode](NullOldHeapNode$))
                case _ => refToObj
              }
              this.copy(refToObj = refMap).pruneUnreachableHeap() // return the current state with updated refToObj
            } else this.bottom() // return the bottom state
        }

      case ReferenceComparisonExpression(left, right, ArithmeticOperator.!=, typ) =>
        (left, right) match {
          case (left: Identifier, right: Identifier) =>
            val l = left match {
              case left: VariableIdentifier => refToObj.getOrElse(left,Set[OldHeapNode]())
              case left: HeapAccess => objToObj.getOrElse(left.obj,Map[String,Set[OldHeapNode]]()).
                getOrElse(left.field,Set[OldHeapNode]())
              case _ => Set[OldHeapNode]()
            }
            val r = right match {
              case right: VariableIdentifier => refToObj.getOrElse(right,Set[OldHeapNode]())
              case right: HeapAccess => objToObj.getOrElse(right.obj,Map[String,Set[OldHeapNode]]()).
                getOrElse(right.field,Set[OldHeapNode]())
              case _ => Set[OldHeapNode]()
            }
            val intersection = l intersect r
            val lr = l diff intersection
            val rl = r diff intersection
            if (lr.isEmpty && rl.isEmpty && intersection.size == 1 && intersection.head.representsSingleVariable)
              this.bottom() // return the bottom state
            else { // there is at least one different heap node
            var refMap = refToObj
              refMap = left match {
                case left: VariableIdentifier => refMap + (left -> lr)
                case _ => refMap
              }
              refMap = right match {
                case right: VariableIdentifier => refMap + (right -> rl)
                case _ => refMap
              }
              var objMap = objToObj
              objMap = left match {
                case left: HeapAccess => objMap + (left.obj -> Map[String,Set[OldHeapNode]](left.field -> lr))
                case _ => objMap
              }
              objMap = right match {
                case right: HeapAccess => objMap + (right.obj -> Map[String,Set[OldHeapNode]](right.field -> rl))
                case _ => objMap
              }
              // return the current state with updated refToObj and objToObj
              this.copy(refToObj = refMap, objToObj = objMap).pruneUnreachableHeap()
            }
          case (left: Identifier, Constant("null",_,_)) =>
            val l = left match {
              case left: VariableIdentifier => refToObj.getOrElse(left,Set[OldHeapNode]())
              case left: HeapAccess => objToObj.getOrElse(left.obj,Map[String,Set[OldHeapNode]]()).
                getOrElse(left.field,Set[OldHeapNode]())
              case _ => Set[OldHeapNode]()
            }
            val r = Set[OldHeapNode](NullOldHeapNode$)
            val intersection = l intersect r
            val lr = l diff intersection
            if (lr.isEmpty && intersection.size == 1 && intersection.head.representsSingleVariable)
              this.bottom()
            else {
              val refMap = left match {
                case left: VariableIdentifier => refToObj + (left -> lr)
                case _ => refToObj
              }
              this.copy(refToObj = refMap).pruneUnreachableHeap() // return the current state with updated refToObj
            }
          case (Constant("null",_,_), right: Identifier) =>
            val l = Set[OldHeapNode](NullOldHeapNode$)
            val r = right match {
              case right: VariableIdentifier => refToObj.getOrElse(right,Set[OldHeapNode]())
              case right: HeapAccess => objToObj.getOrElse(right.obj,Map[String,Set[OldHeapNode]]()).
                getOrElse(right.field,Set[OldHeapNode]())
              case _ => Set[OldHeapNode]()
            }
            val intersection = l intersect r
            val rl = r diff intersection
            if (rl.isEmpty && intersection.size == 1 && intersection.head.representsSingleVariable)
              this.bottom()
            else {
              val refMap = right match {
                case right: VariableIdentifier => refToObj + (right -> rl)
                case _ => refToObj
              }
              this.copy(refToObj = refMap).pruneUnreachableHeap() // return the current state with updated refToObj
            }
        }

      case _ => throw new NotImplementedError("An assume implementation is missing.")
    }
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): S = {
    logger.trace("\n*** ----------------before(" + pp.toString + "): " + this.repr)
    this.copy(currentPP = pp) // return the current state with updated currentPP
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): S = {
    val fields = Set[(Type,String)]()
    val currentPP = DummyProgramPoint
    val temp = 1
    // return a new state with bottom exprSet, empty refToObj, empty objToObj, bottom numDom
    val expr = exprSet.bottom()
    val refToObj = Map[VariableIdentifier,Set[OldHeapNode]]()
    val objToObj = Map[OldHeapNode,Map[String,Set[OldHeapNode]]]()
    val num = numDom.bottom()
    this.copy(fields,currentPP,true,temp,expr,refToObj,objToObj,num)
  }

  def copy(fieldSet: Set[(Type, String)] = fieldSet,
           currentPP: ProgramPoint = currentPP,
           flag: Boolean = flag,
           nonce: Int = nonce,
           exprSet: ExpressionSet = exprSet,
           refToObj: Map[VariableIdentifier, Set[OldHeapNode]] = refToObj,
           objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]] = objToObj,
           numDom: T = numDom): S

  /** Creates an object at allocation site.
    *
    * Invoked by calls to `new()`.
    *
    * @param typ The dynamic type of the created object
    * @param pp The allocation site of the object
    * @return The abstract state with updated exprSet after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): S = {
    logger.trace("*** ----------------createObject(" + typ.toString + "; " + pp.toString + ")")

    var temp = nonce
    var objMap = objToObj
    var num = numDom

    if (!objMap.contains(SummaryOldHeapNode$)) { // the summary heap node was never created before
      // prepare fields to add to objToObj map and add variables to numDom
      var fieldMap = Map[String, Set[OldHeapNode]]()
      for (f <- fieldSet) { // for all fields declared within the program...
        f._1 match {
          case _: RefType => fieldMap = fieldMap + (f._2 -> (objMap.keySet ++ Set[OldHeapNode](SummaryOldHeapNode$, NullOldHeapNode$)))
          case _ => num = num.createVariable(HeapAccess(SummaryOldHeapNode$,f._2,f._1),f._1)
        }
      }
      objMap = objMap + (SummaryOldHeapNode$ -> fieldMap) // add key to objMap
    }
    if (flag) { // materialization is allowed
      val freshR = OldHeapNode(temp); temp = temp + 1 // create fresh heap node
      // prepare fields to add to objToObj and add variables to numDom
      var fieldMap = Map[String,Set[OldHeapNode]]()
      for (f <- fieldSet) { // for all fields declared within the program...
        f._1 match {
          case _:RefType => fieldMap = fieldMap + (f._2 -> Set[OldHeapNode](NullOldHeapNode$))
          case _ => num = num.createVariable(HeapAccess(freshR,f._2,f._1),f._1)
        }
      }
      objMap = objMap + (freshR -> fieldMap) // add key to objMap
      // return the current state with updated exprSet, objToObj, numDom
      this.copy(nonce = temp, exprSet = ExpressionSet(freshR), objToObj = objMap, numDom = num)
    } else { // materialization is forbidden
      // forget everything about the summary heap node
      var fieldMap = Map[String,Set[OldHeapNode]]()
      for (f <- fieldSet) { // for all fields declared within the program...
        f._1 match {
          case _:RefType => fieldMap = fieldMap + (f._2 -> (objMap(SummaryOldHeapNode$)(f._2) ++ Set[OldHeapNode](NullOldHeapNode$)))
          case _ => num = num.setToTop(HeapAccess(SummaryOldHeapNode$,f._2,f._1))
        }
      }
      objMap = objMap + (SummaryOldHeapNode$ -> fieldMap) // add key to objMap
      // return the current state with updated exprSet, objToObj, numDom
      this.copy(nonce = temp, exprSet = ExpressionSet(SummaryOldHeapNode$), objToObj = objMap, numDom = num)
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
    logger.trace("*** ----------------createVariable(" + x.toString + "; " + typ.toString + "; " + pp.toString + ")")

    if (typ.isObject) { // the variable to be created is a Ref
      val refMap = refToObj + (x -> Set[OldHeapNode](NullOldHeapNode$)) // add key to refToObj map
      // return the current state with updated exprSet and refToObj
      this.copy(exprSet = ExpressionSet(x), refToObj = refMap)
    } else { // the variable to be created is not a `Ref`
      this.copy(numDom = numDom.createVariable(x,typ))  // return the current state with updated numDom
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
    logger.trace("*** ----------------createVariableForArgument(" + x.toString + "; " + typ.toString + ")")

    if (typ.isObject) { // the variable to be created is a Ref
      if (objToObj.contains(SummaryOldHeapNode$)) { // the summary heap node exists already
        // add key to refToObj map
        val refMap = refToObj + (x -> Set[OldHeapNode](SummaryOldHeapNode$, NullOldHeapNode$))
        // return the current state with updated refToObj and objToObj
        this.copy(exprSet = ExpressionSet(x), refToObj = refMap)
      } else { // the summary heap node was never created before
        // prepare fields to add to objToObj map and add variables to numDom
        var fieldMap = Map[String,Set[OldHeapNode]]()
        var num = numDom
        for (f <- fieldSet) { // for all fields declared within the program...
          f._1 match {
            case _:RefType =>
              fieldMap = fieldMap + (f._2 -> Set[OldHeapNode](SummaryOldHeapNode$, NullOldHeapNode$))
            case _ =>
              num = num.createVariable(HeapAccess(SummaryOldHeapNode$,f._2,f._1),f._1)
          }
        }
        // add key to refToObj map
        val refMap = refToObj + (x -> Set[OldHeapNode](SummaryOldHeapNode$, NullOldHeapNode$))
        // add key to objToObj map
        val objMap = objToObj + (SummaryOldHeapNode$ -> fieldMap)
        // return the current state with updated exprSet, refToObj, objToObj and numDom
        this.copy(exprSet = ExpressionSet(x), refToObj = refMap, objToObj = objMap, numDom = num)
      }
    } else { // the variable to be created is not a Ref
      this.copy(numDom = numDom.createVariable(x,typ))  // return the current state with updated numDom
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
    // return the current state with updated exprSet
    this.copy(exprSet = ExpressionSet(new Constant(value, typ, pp)))
  }

  /** The current expression.
    *
    * Invoked after each statement to retrieve its result.
    */
  override def expr: ExpressionSet = this.exprSet

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): S = {
    val fields = Set[(Type,String)]()
    val currentPP = DummyProgramPoint
    val nonce = 1
    // return a new state with factory exprSet, empty refToObj, empty objToObj, factory numDom
    val expr = ExpressionSet()
    val refToObj = Map[VariableIdentifier,Set[OldHeapNode]]()
    val objToObj = Map[OldHeapNode,Map[String,Set[OldHeapNode]]]()
    val num = numDom.factory()
    this.copy(fields,currentPP,true,nonce,expr,refToObj,objToObj,num)
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
    logger.trace("*** ----------------getFieldValue(" + obj.toString + "; " + field + "; " + typ.toString + ")")

    obj match {
      case obj:AccessPathIdentifier =>
        val path: List[Identifier] = obj.path // path to evaluate

        // path head evaluation
        var temp = nonce
        var refMap = refToObj
        var objMap = objToObj
        var num = numDom

        val head = path.head.asInstanceOf[VariableIdentifier]
        var rcvSet = refToObj(head)  // initial receiver set
        if (rcvSet.contains(SummaryOldHeapNode$) && flag) { // materialization
          val freshR = OldHeapNode(temp); temp = temp + 1 // create fresh heap node
          rcvSet = rcvSet - SummaryOldHeapNode$ + freshR // update receiver set
          refMap = refMap + (head -> rcvSet) // add key to refMap to replace the summary node with the fresh node
          // update refMap adding the fresh node where the summary node is present
          refMap = refMap.mapValues(s => if (s.contains(SummaryOldHeapNode$)) s + freshR else s)
          // update objMap adding the fresh node where the summary node is present
          objMap = objMap.mapValues(
            m => m.mapValues(s => if (s.contains(SummaryOldHeapNode$)) s + freshR else s)
          )
          objMap = objMap + (freshR -> objMap(SummaryOldHeapNode$).mapValues(s => s - freshR)) // add key to objMap
          for (f <- fieldSet) { // for all fields declared within the program...
            f._1 match {
              case _:RefType =>
              case _ =>
                val sum = HeapAccess(SummaryOldHeapNode$,f._2,f._1)
                num = num.rename(List[HeapAccess](sum),List[HeapAccess](HeapAccess(freshR,f._2,f._1)))
                num = num.createVariable(sum,f._1)
            }
          }
        }

        // path tail evaluation
        val eval = path.tail.dropRight(1).foldLeft((refMap,objMap,num,rcvSet))(
          (curr,next) => {
            var rM: Map[VariableIdentifier,Set[OldHeapNode]] = curr._1
            var oM: Map[OldHeapNode,Map[String,Set[OldHeapNode]]] = curr._2
            var nD: T = curr._3
            var rS = Set[OldHeapNode]()
            for (obj: OldHeapNode <- curr._4) { // for all current receivers...
              var tR = oM.getOrElse(obj,Map[String,Set[OldHeapNode]]()).getOrElse(next.getName,Set[OldHeapNode]())
              if (tR.contains(SummaryOldHeapNode$) && flag) { // materialization
                val freshO = OldHeapNode(temp); temp = temp + 1 // create fresh heap node
                tR = tR - SummaryOldHeapNode$ + freshO // update receiver set
                // update rM adding the fresh node where the summary node is present
                rM = rM.mapValues(s => if (s.contains(SummaryOldHeapNode$)) s + freshO else s)
                // add key to oM to replace the summary node with the fresh node
                oM = oM + (obj -> (oM.getOrElse(obj,Map[String,Set[OldHeapNode]]()) + (next.getName -> tR)))
                // update oM adding the fresh node where the summary node is present
                oM = oM.mapValues(
                  m => m.mapValues(s => if (s.contains(SummaryOldHeapNode$)) s + freshO else s)
                )
                oM = oM + (freshO -> oM(SummaryOldHeapNode$).mapValues(s => s - freshO)) // add key to oM
                for (f <- fieldSet) { // for all fields declared within the program...
                  f._1 match {
                    case _:RefType =>
                    case _ =>
                      val sum = HeapAccess(SummaryOldHeapNode$,f._2,f._1)
                      nD = nD.rename(List[HeapAccess](sum),List[HeapAccess](HeapAccess(freshO,f._2,f._1)))
                      nD = nD.createVariable(sum,f._1)
                  }
                }
              }
              rS = rS ++ tR
            }
            (rM,oM,nD,rS)
          }
        )

        refMap = eval._1
        objMap = eval._2
        num = eval._3
        // path end evaluation
        if (typ.isObject) { // the accessed field is a Ref
          for (obj: OldHeapNode <- eval._4 - NullOldHeapNode$) { // for all current receivers...
            rcvSet = objMap.getOrElse(obj,Map[String,Set[OldHeapNode]]()).getOrElse(field,Set[OldHeapNode]())
            if (rcvSet.contains(SummaryOldHeapNode$) && flag) { // materialization
              val freshE = OldHeapNode(temp); temp = temp + 1 // create fresh heap node
              rcvSet = rcvSet - SummaryOldHeapNode$ + freshE // update receiver set
              // update refMap adding the fresh node where the summary node is present
              refMap = refMap.mapValues(s => if (s.contains(SummaryOldHeapNode$)) s + freshE else s)
              // add key to objMap to replace the summary node with the fresh node
              objMap = objMap + (obj -> (objMap.getOrElse(obj,Map[String,Set[OldHeapNode]]()) + (field -> rcvSet)))
              // update objMap adding the fresh node where the summary node is present
              objMap = objMap.mapValues(
                m => m.mapValues(s => if (s.contains(SummaryOldHeapNode$)) s + freshE else s)
              )
              objMap = objMap + (freshE -> objMap(SummaryOldHeapNode$).mapValues(s => s - freshE)) // add key to objMap
              for (f <- fieldSet) { // for all fields declared within the program...
                f._1 match {
                  case _:RefType =>
                  case _ =>
                    val sum = HeapAccess(SummaryOldHeapNode$,f._2,f._1)
                    num = num.rename(List[HeapAccess](sum),List[HeapAccess](HeapAccess(freshE,f._2,f._1)))
                    num = num.createVariable(sum,f._1)
                }
              }
            }
          }
        }

        // null pointer dereference warning report
        if (eval._4.contains(NullOldHeapNode$)) Reporter.reportInfo("Possible null pointer dereference", currentPP)
        rcvSet = eval._4 - NullOldHeapNode$
        val expr = rcvSet.foldLeft(ExpressionSet())(
          (exp,rcv) =>  exp add ExpressionSet(HeapAccess(rcv,field,typ))  // create new HeapAccess
        )
        // return the current state with updated exprSet, refToObj, objToObj, numDom
        this.copy(nonce = temp, exprSet = expr, refToObj = refMap, objToObj = objMap, numDom = num)
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
    logger.trace("*** ----------------getVariableValue(" + id.toString + ")")

    var temp = nonce
    var refMap = refToObj
    var objMap = objToObj
    var num = numDom

    val x = id.asInstanceOf[VariableIdentifier]
    var rcvSet = refToObj.getOrElse(x,Set[OldHeapNode]())  // receiver set
    if (rcvSet.contains(SummaryOldHeapNode$) && flag) { // materialization
      val freshR = OldHeapNode(temp); temp = temp + 1 // create fresh heap node
      rcvSet = rcvSet - SummaryOldHeapNode$ + freshR // update receiver set
      refMap = refMap + (x -> rcvSet) // add key to refMap to replace the summary node with the fresh node
      // update refMap adding the fresh node where the summary node is present
      refMap = refMap.mapValues(s => if (s.contains(SummaryOldHeapNode$)) s + freshR else s)
      // update objMap adding the fresh node where the summary node is present
      objMap = objMap.mapValues(
        m => m.mapValues(s => if (s.contains(SummaryOldHeapNode$)) s + freshR else s)
      )
      objMap = objMap + (freshR -> objMap(SummaryOldHeapNode$).mapValues(s => s - freshR)) // add key to objMap
      for (f <- fieldSet) { // for all fields declared within the program...
        f._1 match {
          case _:RefType =>
          case _ =>
            val sum = HeapAccess(SummaryOldHeapNode$,f._2,f._1)
            num = num.rename(List[HeapAccess](sum),List[HeapAccess](HeapAccess(freshR,f._2,f._1)))
            num = num.createVariable(sum,f._1)
        }
      }
    }

    // return the current state with updated exprSet, refToObj, objToObj, numDom
    this.copy(nonce = temp, exprSet = ExpressionSet(id), refToObj = refMap, objToObj = objMap, numDom = num)
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: S): S = {
    logger.trace("*** glb(" + this.repr + ", " + other.repr + ")")

    def zipper[K](map1: Map[K,Set[OldHeapNode]], map2: Map[K,Set[OldHeapNode]]): Map[K,Set[OldHeapNode]] = {
      var keyToObjmap = Map[K,Set[OldHeapNode]]()
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,_) =>
          case (_,None) =>
          case (Some(o1),Some(o2)) => keyToObjmap = keyToObjmap + (key -> (o1 & o2))
        }
      }; keyToObjmap
    }
    val fields = this.fieldSet & other.fieldSet  // meet the fieldSets
    val allowed = this.flag && other.flag // meet the materialization flags
    val temp = Math.min(this.nonce, other.nonce) // take the minimum nonce
    val expr = this.exprSet glb other.expr // join the exprSets
    val refMap = zipper[VariableIdentifier](this.refToObj,other.refToObj)  // merge the refToObjs
    // merge the objToObj
    var objMap = Map[OldHeapNode,Map[String,Set[OldHeapNode]]]()
    for (key <- this.objToObj.keySet ++ other.objToObj.keySet) { // for all keys present in either map...
      (this.objToObj.get(key),other.objToObj.get(key)) match {
        case (None,_) =>
        case (_,None) =>
        case (Some(m1: Map[String,Set[OldHeapNode]]),Some(m2: Map[String,Set[OldHeapNode]])) =>
          objMap = objMap + (key -> zipper[String](m1,m2))
      }
    }
    val num = this.numDom glb other.numDom // meet the numDoms
    // return the current state with updated exprSet, refToObj, objToObj and numDom
    this.copy(fieldSet = fields, flag = allowed, nonce = temp, exprSet = expr, refToObj = refMap, objToObj = objMap, numDom = num)
  }

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return `true` if and only if the state is equivalent to bottom
    */
  override def isBottom: Boolean = numDom.isBottom

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return `true` if and only if the state is equivalent to top
    */
  override def isTop: Boolean = numDom.isTop

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return `true` if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: S): Boolean = {
    logger.trace("*** lessEqual(" + this.repr + ", " + other.repr + ")")

    val refMap = this.refToObj.forall {
      case (k: VariableIdentifier,s: Set[OldHeapNode]) => {
        val oo = other.refToObj.getOrElse(k,Set[OldHeapNode]())
        (!oo.contains(SummaryOldHeapNode$) || s.contains(SummaryOldHeapNode$)) &&
          ((s - SummaryOldHeapNode$) subsetOf (oo - SummaryOldHeapNode$))
      }
    } // compare the refToObj
    val objMap = this.objToObj.forall {
        case (o: OldHeapNode, m: Map[String,Set[OldHeapNode]]) => m.forall {
          case (f: String, s: Set[OldHeapNode]) => {
            val oo = other.objToObj.getOrElse(o,Map[String,Set[OldHeapNode]]()).getOrElse(f,Set[OldHeapNode]())
            (!oo.contains(SummaryOldHeapNode$) || s.contains(SummaryOldHeapNode$)) &&
              ((s - SummaryOldHeapNode$) subsetOf (oo - SummaryOldHeapNode$))
          }
        }
      } // test the objToObj
    val num = this.numDom.lessEqual(other.numDom) // compare the numDom
    refMap && objMap && num
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: S): S = {
    logger.trace("*** lub(" + this.repr + ", " + other.repr + ")")

    def zipper[K](map1: Map[K,Set[OldHeapNode]], map2: Map[K,Set[OldHeapNode]]): Map[K,Set[OldHeapNode]] = {
      var keyToObjmap = Map[K,Set[OldHeapNode]]()
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,None) =>
          case (None,Some(o2)) => keyToObjmap = keyToObjmap + (key -> o2)
          case (Some(o1),None) => keyToObjmap = keyToObjmap + (key -> o1)
          case (Some(o1),Some(o2)) => // we keep the summary node only if present in both maps
            val o = (o1 - SummaryOldHeapNode$) ++ (o2 - SummaryOldHeapNode$) ++ (o1 & o2)
            keyToObjmap = keyToObjmap + (key -> o)
        }
      }; keyToObjmap
    }
    val fields = this.fieldSet ++ other.fieldSet  // join the fieldSets
    val allowed = this.flag || other.flag // join the materialization flags
    val temp = Math.max(this.nonce, other.nonce) // take the maximum nonce
    val expr = this.exprSet lub other.expr // join the exprSets
    val refMap = zipper[VariableIdentifier](this.refToObj,other.refToObj)  // merge the refToObjs
    // merge the objToObj
    var objMap = Map[OldHeapNode,Map[String,Set[OldHeapNode]]]()
    for (key <- this.objToObj.keySet ++ other.objToObj.keySet) { // for all keys present in either map...
      (this.objToObj.get(key),other.objToObj.get(key)) match {
        case (None,None) =>
        case (None,Some(m2)) => objMap = objMap + (key -> m2)
        case (Some(m1),None) => objMap = objMap + (key -> m1)
        case (Some(m1: Map[String,Set[OldHeapNode]]),Some(m2: Map[String,Set[OldHeapNode]])) =>
          objMap = objMap + (key -> zipper[String](m1,m2))
      }
    }
    val num = this.numDom lub other.numDom // join the numDoms
    // return the current state with updated exprSet, refToObj, objToObj and numDom
    this.copy(fieldSet = fields, flag = allowed, nonce = temp, exprSet = expr, refToObj = refMap, objToObj = objMap, numDom = num)
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): S = {
    // retrieve all heap nodes reachable from Ref variables
    val objFromRef = refToObj.foldLeft(Set[OldHeapNode]())((r, s) => r ++ s._2)
    // add all Obj reachable from fields of Obj reachable from Ref variables
    var reach = objFromRef
    for (id <- objFromRef) {
      reach = objToObj.getOrElse(id,Map[String,Set[OldHeapNode]]()).foldLeft(reach)((r, s) => r ++ s._2)
    }
    // collect and remove all unreachable Obj
    var unreach = Set[OldHeapNode]()
    var objMap = objToObj
    for (key <- objToObj.keys) {
      if (!reach.contains(key)) { objMap = objMap - key; unreach = unreach + key }
    }
    // prune variables from numDom
    var num = numDom
    for (obj <- unreach) { for (f <- fieldSet) { num = num.removeVariable(HeapAccess(obj,f._2,f._1)) } }
    // return the current state with updated objToObj and numDom
    this.copy(objToObj = objMap, numDom = num)
  }

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): S = {
    logger.debug("*** pruneVariables(" + filter.toString + "): implement me!")

    ???
  }

  /** Removes the current expression.
    *
    * @return The abstract state obtained after removing the current expression
    */
  override def removeExpression(): S = this.copy(exprSet = ExpressionSet())

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
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
    "MayPointToNumericalState(" +
      exprSet.toString + ", " +
      refToObj.toString + ", " +
      objToObj.toString + ", " +
      numDom.toString + ")"
  }

  /** Assigns an expression to an argument.
    *
    * @param x The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
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
  override def setExpression(expr: ExpressionSet): S = this.copy(exprSet = expr)

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  override def setVariableToTop(varExpr: Expression): S = {
    logger.debug("*** setVariableToTop(" + varExpr.toString + "): implement me!")

    ???
  }

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
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
    val fields = Set[(Type,String)]()
    val currentPP = DummyProgramPoint
    val nonce = 0
    // return a new state with top exprSet, empty refToObj, empty objToObj, top numDom
    val expr = exprSet.top()
    val refToObj = Map[VariableIdentifier,Set[OldHeapNode]]()
    val objToObj = Map[OldHeapNode,Map[String,Set[OldHeapNode]]]()
    val num = numDom.top()
    this.copy(fields,currentPP,true,nonce,expr,refToObj,objToObj,num)
  }

  /** The state string representation.
    *
    * @return the string representation of the current state
    */
  override def toString: String = {
    "MayPointToNumericalState(\n" +
      "\texprSet: " + exprSet.toString + "\n" +
      "\trefToObj: " + refToObj.toString + "\n" +
      "\tobjToObj: " + objToObj.toString + "\n" +
      "\tnumDom: " + numDom.toString + "\n" +
      ")"
  }

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: S): S = {
    logger.trace("*** ----------------widening(" + other.repr + ")")

    def zipper[K](map1: Map[K,Set[OldHeapNode]], map2: Map[K,Set[OldHeapNode]]): Map[K,Set[OldHeapNode]] = {
      var keyToObjmap = Map[K,Set[OldHeapNode]]()
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,None) =>
          case (None,Some(o2)) => keyToObjmap = keyToObjmap + (key -> o2)
          case (Some(o1),None) => keyToObjmap = keyToObjmap + (key -> o1)
          case (Some(o1),Some(o2)) => // we keep the summary node only if present in both maps
            val o = (o1 - SummaryOldHeapNode$) ++ (o2 - SummaryOldHeapNode$) ++ (o1 & o2)
            keyToObjmap = keyToObjmap + (key -> o)
        }
      }; keyToObjmap
    }
    val fields = this.fieldSet ++ other.fieldSet  // join the fieldSets
    val temp = Math.max(this.nonce, other.nonce) // take the maximum nonce
    val expr = this.exprSet widening other.expr // widen the exprSets
    val refMap = zipper[VariableIdentifier](this.refToObj,other.refToObj)  // merge the refToObjs
    // merge the objToObj
    var objMap = Map[OldHeapNode,Map[String,Set[OldHeapNode]]]()
    for (key <- this.objToObj.keySet ++ other.objToObj.keySet) { // for all keys present in either map...
      (this.objToObj.get(key),other.objToObj.get(key)) match {
        case (None,None) =>
        case (None,Some(m2)) => objMap = objMap + (key -> m2)
        case (Some(m1),None) => objMap = objMap + (key -> m1)
        case (Some(m1: Map[String,Set[OldHeapNode]]),Some(m2: Map[String,Set[OldHeapNode]])) =>
          objMap = objMap + (key -> zipper[String](m1,m2))
      }
    }
    val num = this.numDom widening other.numDom // widen the numDoms
    // return the current state with updated exprSet, refToObj, objToObj and numDom
    this.copy(fieldSet = fields, flag = false, nonce = temp, exprSet = expr, refToObj = refMap, objToObj = objMap, numDom = num)
  }
}

/** MayPointTo+Intervals Analysis State.
  *
  * @param fieldSet fields declared within the program
  * @param currentPP current program point
  * @param nonce freshly generated heap node
  * @param exprSet result of previous statement
  * @param refToObj map from Ref variables to heap objects
  * @param objToObj map from heap objects to a map from Ref fields to heap objects
  * @param numDom intervals abstract domain
  * @author Caterina Urban
  */
case class MayPointToIntervalsState(fieldSet: Set[(Type, String)],
                                    currentPP: ProgramPoint,
                                    flag: Boolean,
                                    nonce: Int,
                                    exprSet: ExpressionSet,
                                    refToObj: Map[VariableIdentifier, Set[OldHeapNode]],
                                    objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]],
                                    numDom: NumDom.I)
  extends MayPointToNumericalState[NumDom.I,MayPointToIntervalsState] {
  override def copy(fieldSet: Set[(Type, String)],
                    currentPP: ProgramPoint,
                    flag: Boolean,
                    nonce: Int,
                    exprSet: ExpressionSet,
                    refToObj: Map[VariableIdentifier, Set[OldHeapNode]],
                    objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]],
                    numDom: NumDom.I): MayPointToIntervalsState =
    MayPointToIntervalsState(fieldSet, currentPP, flag, nonce, exprSet, refToObj, objToObj, numDom)
}

/** MayPointTo+Octagons Analysis State.
  *
  * @param fieldSet fields declared within the program
  * @param currentPP current program point
  * @param nonce freshly generated heap node
  * @param exprSet result of previous statement
  * @param refToObj map from Ref variables to heap objects
  * @param objToObj map from heap objects to a map from Ref fields to heap objects
  * @param numDom octagons abstract domain
  * @author Caterina Urban
  */
case class MayPointToIntegerOctagonsState(fieldSet: Set[(Type, String)],
                                          currentPP: ProgramPoint,
                                          flag: Boolean,
                                          nonce: Int,
                                          exprSet: ExpressionSet,
                                          refToObj: Map[VariableIdentifier, Set[OldHeapNode]],
                                          objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]],
                                          numDom: NumDom.IO)
  extends MayPointToNumericalState[NumDom.IO,MayPointToIntegerOctagonsState] {
  override def copy(fieldSet: Set[(Type, String)],
                    currentPP: ProgramPoint,
                    flag: Boolean,
                    nonce: Int,
                    exprSet: ExpressionSet,
                    refToObj: Map[VariableIdentifier, Set[OldHeapNode]],
                    objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]],
                    numDom: NumDom.IO): MayPointToIntegerOctagonsState =
    MayPointToIntegerOctagonsState(fieldSet, currentPP, flag, nonce, exprSet, refToObj, objToObj, numDom)
}


/** MayPointTo+Octagons Analysis State.
  *
  * @param fieldSet fields declared within the program
  * @param currentPP current program point
  * @param nonce freshly generated heap node
  * @param exprSet result of previous statement
  * @param refToObj map from Ref variables to heap objects
  * @param objToObj map from heap objects to a map from Ref fields to heap objects
  * @param numDom octagons abstract domain
  * @author Caterina Urban
  */
case class MayPointToDoubleOctagonsState(fieldSet: Set[(Type, String)],
                                          currentPP: ProgramPoint,
                                          flag: Boolean,
                                          nonce: Int,
                                          exprSet: ExpressionSet,
                                          refToObj: Map[VariableIdentifier, Set[OldHeapNode]],
                                          objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]],
                                          numDom: NumDom.DO)
  extends MayPointToNumericalState[NumDom.DO, MayPointToDoubleOctagonsState] {
  override def copy(fieldSet: Set[(Type, String)],
                    currentPP: ProgramPoint,
                    flag: Boolean,
                    nonce: Int,
                    exprSet: ExpressionSet,
                    refToObj: Map[VariableIdentifier, Set[OldHeapNode]],
                    objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]],
                    numDom: NumDom.DO): MayPointToDoubleOctagonsState =
    MayPointToDoubleOctagonsState(fieldSet, currentPP, flag, nonce, exprSet, refToObj, objToObj, numDom)
}


/** MayPointTo+Octagons Analysis State.
  *
  * @param fieldSet fields declared within the program
  * @param currentPP current program point
  * @param nonce freshly generated heap node
  * @param exprSet result of previous statement
  * @param refToObj map from Ref variables to heap objects
  * @param objToObj map from heap objects to a map from Ref fields to heap objects
  * @param numDom (apron) octagons abstract domain
  * @author Caterina Urban
  */
case class MayPointToAOctagonsState(fieldSet: Set[(Type, String)],
                                    currentPP: ProgramPoint,
                                    flag: Boolean,
                                    nonce: Int,
                                    exprSet: ExpressionSet,
                                    refToObj: Map[VariableIdentifier, Set[OldHeapNode]],
                                    objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]],
                                    numDom: NumDom.AO)
  extends MayPointToNumericalState[NumDom.AO,MayPointToAOctagonsState] {
  override def copy(fieldSet: Set[(Type, String)],
                    currentPP: ProgramPoint,
                    flag: Boolean,
                    nonce: Int,
                    exprSet: ExpressionSet,
                    refToObj: Map[VariableIdentifier, Set[OldHeapNode]],
                    objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]],
                    numDom: NumDom.AO): MayPointToAOctagonsState =
    MayPointToAOctagonsState(fieldSet, currentPP, flag, nonce, exprSet, refToObj, objToObj, numDom)
}

/** MayPointTo+Polyhedra Analysis State.
  *
  * @param fieldSet fields declared within the program
  * @param currentPP current program point
  * @param nonce freshly generated heap node
  * @param exprSet result of previous statement
  * @param refToObj map from Ref variables to heap objects
  * @param objToObj map from heap objects to a map from Ref fields to heap objects
  * @param numDom (apron) polyhedra abstract domain
  * @author Caterina Urban
  */
case class MayPointToAPolyhedraState(fieldSet: Set[(Type, String)],
                                     currentPP: ProgramPoint,
                                     flag: Boolean,
                                     nonce: Int,
                                     exprSet: ExpressionSet,
                                     refToObj: Map[VariableIdentifier, Set[OldHeapNode]],
                                     objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]],
                                     numDom: NumDom.AP)
  extends MayPointToNumericalState[NumDom.AP,MayPointToAPolyhedraState] {
  override def copy(fieldSet: Set[(Type, String)],
                    currentPP: ProgramPoint,
                    flag: Boolean,
                    nonce: Int,
                    exprSet: ExpressionSet,
                    refToObj: Map[VariableIdentifier, Set[OldHeapNode]],
                    objToObj: Map[OldHeapNode, Map[String, Set[OldHeapNode]]],
                    numDom: NumDom.AP): MayPointToAPolyhedraState =
    MayPointToAPolyhedraState(fieldSet, currentPP, flag, nonce, exprSet, refToObj, objToObj, numDom)
}

/** MayPointTo+Numerical Analysis Entry State.
  *
  * @tparam T the numerical domain
  * @tparam S the maypointto+numerical state
  * @author Caterina Urban
  */
trait MayPointToNumericalEntryStateBuilder[T <: NumericalDomain[T], S <: MayPointToNumericalState[T,S]] extends ForwardEntryStateBuilder[S] {

  protected var fields: Set[(Type,String)] = Set[(Type,String)]()

  override def build(method: MethodDeclaration): S = {
    fields = Set[(Type,String)]()
    for(f <- method.classDef.fields) {
      fields = fields + ((f.typ, f.variable.toString))
    }
    method.initializeArgument[S](topState.copy(fieldSet = fields))
  }

}

/** MayPointTo+Intervals Analysis Entry States.
  *
  * @author Caterina Urban
  */
object MayPointToIntervalsEntryStateBuilder
  extends MayPointToNumericalEntryStateBuilder[NumDom.I, MayPointToIntervalsState] {

  override def topState = MayPointToIntervalsState(fields, DummyProgramPoint, true, 1,
    ExpressionSet(),
    Map[VariableIdentifier,Set[OldHeapNode]](),
    Map[OldHeapNode,Map[String,Set[OldHeapNode]]](),
    new BoxedNonRelationalNumericalDomain[DoubleInterval](DoubleInterval.Top))
}

/** MayPointTo+Octagons Analysis Entry States.
  *
  * @author Caterina Urban
  */
object MayPointToIntegerOctagonsEntryStateBuilder
  extends MayPointToNumericalEntryStateBuilder[NumDom.IO, MayPointToIntegerOctagonsState] {

  override def topState = MayPointToIntegerOctagonsState(fields, DummyProgramPoint, true, 1,
    ExpressionSet(),
    Map[VariableIdentifier,Set[OldHeapNode]](),
    Map[OldHeapNode,Map[String,Set[OldHeapNode]]](),
    IntegerOctagons.Bottom.factory)
}

object MayPointToDoubleOctagonsEntryStateBuilder
  extends MayPointToNumericalEntryStateBuilder[NumDom.DO, MayPointToDoubleOctagonsState] {

  override def topState = MayPointToDoubleOctagonsState(fields, DummyProgramPoint, true, 1,
    ExpressionSet(),
    Map[VariableIdentifier,Set[OldHeapNode]](),
    Map[OldHeapNode,Map[String,Set[OldHeapNode]]](),
    DoubleOctagons.Bottom.factory)
}


/** MayPointTo+Octagons Analysis Entry States.
  *
  * @author Caterina Urban
  */
object MayPointToAOctagonsEntryStateBuilder
  extends MayPointToNumericalEntryStateBuilder[NumDom.AO, MayPointToAOctagonsState] {

  override def topState = MayPointToAOctagonsState(fields, DummyProgramPoint, true, 1,
    ExpressionSet(),
    Map[VariableIdentifier,Set[OldHeapNode]](),
    Map[OldHeapNode,Map[String,Set[OldHeapNode]]](),
    Apron.Octagons.Bottom.factory)
}

/** MayPointTo+Polyhedra Analysis Entry States.
  *
  * @author Caterina Urban
  */
object MayPointToAPolyhedraEntryStateBuilder
  extends MayPointToNumericalEntryStateBuilder[NumDom.AP, MayPointToAPolyhedraState] {

  override def topState = MayPointToAPolyhedraState(fields, DummyProgramPoint, true, 1,
    ExpressionSet(),
    Map[VariableIdentifier,Set[OldHeapNode]](),
    Map[OldHeapNode,Map[String,Set[OldHeapNode]]](),
    Apron.Polyhedra.Bottom.factory)
}

/** MayPointTo+Numerical Analysis Runner.
  *
  * @tparam N the numerical domain
  * @tparam T the maypointto+numerical state
  * @author Caterina Urban
  */
trait MayPointToNumericalAnalysisRunner[N <: NumericalDomain[N], T <: MayPointToNumericalState[N,T]] extends SilverAnalysisRunner[T] {

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

/** MayPointTo+Intervals Analysis Runner.
  *
  * @author Caterina Urban
  */
object MayPointToIntervalsAnalysisRunner
  extends MayPointToNumericalAnalysisRunner[BoxedNonRelationalNumericalDomain[DoubleInterval], MayPointToIntervalsState] {
  override val analysis = SimpleForwardAnalysis[MayPointToIntervalsState](MayPointToIntervalsEntryStateBuilder)
  override def toString = "PointsTo+Intervals Analysis"
}

/** MayPointTo+Octagons Analysis Runner.
  *
  * @author Caterina Urban
  */
object MayPointToIntegerOctagonsAnalysisRunner
  extends MayPointToNumericalAnalysisRunner[IntegerOctagons, MayPointToIntegerOctagonsState] {
  override val analysis = SimpleForwardAnalysis[MayPointToIntegerOctagonsState](MayPointToIntegerOctagonsEntryStateBuilder)
  override def toString = "PointsTo+Octagons Analysis"
}

/** MayPointTo+Octagons Analysis Runner.
  *
  * @author Caterina Urban
  */
object MayPointToAOctagonsAnalysisRunner
  extends MayPointToNumericalAnalysisRunner[Apron.Octagons, MayPointToAOctagonsState] {
  override val analysis = SimpleForwardAnalysis[MayPointToAOctagonsState](MayPointToAOctagonsEntryStateBuilder)
  override def toString = "PointsTo+Octagons Analysis"
}


/** MayPointTo+Polyhedra Analysis Runner.
  *
  * @author Caterina Urban
  */
object MayPointToAPolyhedraAnalysisRunner
  extends MayPointToNumericalAnalysisRunner[Apron.Polyhedra, MayPointToAPolyhedraState] {
  override val analysis = SimpleForwardAnalysis[MayPointToAPolyhedraState](MayPointToAPolyhedraEntryStateBuilder)
  override def toString = "MayPointTo+Polyhedra Analysis"
}
