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
import ch.ethz.inf.pm.sample.execution.{EntryStateBuilder, SimpleAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver._
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

/** Field access.
  *
  * @param rcv the receiver heap node
  * @param field the field name
  * @param typ the field type
  * @author Caterina Urban
  */
case class HeapAccess(rcv: HeapNode, field: String, typ: Type) extends Identifier {
  override def getName: String = rcv.getName + "." + field
  override def getField: Option[String] = Some(field)
  override def pp: ProgramPoint = rcv.pp
  override def representsSingleVariable: Boolean = rcv.representsSingleVariable
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
  extends SimpleState[S] with StateWithBackwardAnalysisStubs[S] with LazyLogging
{
  this: S =>

  def fieldSet: Set[(Type,String)] // fields declared within the program

  def currentPP: ProgramPoint // current program point
  def flag: Boolean // true = materialization allowed; false = materialization not allowed
  def nonce: Int // fresh number for newly created heap nodes

  def exprSet: ExpressionSet // result of previous statement
  // map from Ref variables to heap objects
  def refToObj: Map[VariableIdentifier,Set[HeapNode]]
  // map from heap objects to a map from Ref fields to heap objects
  def objToObj: Map[HeapNode,Map[String,Set[HeapNode]]]
  def numDom: T // numerical abstract domain

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
              val s: Set[HeapNode] = objToObj(right.rcv)(right.field) // retrieve the heap Obj objects
              val o: HeapNode = obj.rcv // retrieve `Obj` whose field is assigned
              val f: String = obj.field // retrieve assigned field
              val objMap = if (o.representsSingleVariable) { // strong update
                objToObj + (o -> (objToObj(o) + (f -> s)))
              } else { // weak update
                objToObj + (o -> (objToObj(o) + (f -> (objToObj(o)(f) ++ s))))
              }
              // return the current state with updated objFieldToObj
              this.copy(objToObj = objMap).pruneUnreachableHeap()

            case right: Constant => // e.g., `x.f := null`
              val o = obj.rcv // retrieve `Obj` whose field is assigned
              val f = obj.field // retrieve assigned field
              // weak update
              val objMap = if (o.representsSingleVariable) { // strong update
                objToObj + (o -> (objToObj(o) + (f -> Set[HeapNode](NullHeapNode))))
              } else { // weak update
                objToObj + (o -> (objToObj(o) + (f -> (objToObj(o)(f) ++ Set[HeapNode](NullHeapNode)))))
              }
              // return the current state with updated objFieldToObj
              this.copy(objToObj = objMap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x.f := y`
              val s = refToObj(right) // retrieve the corresponding heap `Obj` objects
              val o = obj.rcv // retrieve `Obj` whose field is assigned
              val f = obj.field // retrieve assigned field
              // weak update
              val objMap = if (o.representsSingleVariable) { // strong update
                objToObj + (o -> (objToObj(o) + (f -> s)))
              } else { // weak update
                objToObj + (o -> (objToObj(o) + (f -> (objToObj(o)(f) ++ s))))
              }
              // return the current state with updated objFieldToObj
              this.copy(objToObj = objMap).pruneUnreachableHeap()

            case _ => throw new NotImplementedError("A field assignment implementation is missing.")
          }
        } else {  // the assigned field is not a Ref
          val num = if (obj.rcv.representsSingleVariable) { // strong update
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
    logger.trace("*** ----------------assignVariable(" + x.toString + "; " + right.toString + ")")

    x match {
      case x: VariableIdentifier =>
        if (x.typ.isObject) { // the assigned variable is a Ref
          right match {
            case right: HeapAccess => // e.g., `x := y.g`
              val s = this.objToObj(right.rcv)(right.field) // retrieve the heap `Obj` objects
              // add xref -> s to refToObj map
              val refMap = this.refToObj + (x -> s)
              // return the current state with updated refToObj
              this.copy(refToObj = refMap).pruneUnreachableHeap()

            case right: HeapNode => // e.g., `x = new()`
              // add xref -> right to refToObj map
              val refMap = this.refToObj + (x -> Set[HeapNode](right))
              // return the current state with updated refToObj
              this.copy(refToObj = refMap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x := y`
              // add xref -> refToObj[rightref] to refToObj map
              val refMap = this.refToObj + (x -> this.refToObj.getOrElse(right, Set[HeapNode](NullHeapNode)))
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
    logger.debug("*** ----------------assume(" + cond.toString + ")")

    this //TODO
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
    // return a new state with bottom exprSet, empty refToObj, empty objFieldToObj, bottom numDom
    val expr = exprSet.bottom()
    val refToObj = Map[VariableIdentifier,Set[HeapNode]]()
    val objFieldToObj = Map[HeapNode,Map[String,Set[HeapNode]]]()
    val num = numDom.bottom()
    this.copy(fields,currentPP,true,temp,expr,refToObj,objFieldToObj,num)
  }

  def copy(fieldSet: Set[(Type, String)] = fieldSet,
           currentPP: ProgramPoint = currentPP,
           flag: Boolean = flag,
           nonce: Int = nonce,
           exprSet: ExpressionSet = exprSet,
           refToObj: Map[VariableIdentifier, Set[HeapNode]] = refToObj,
           objToObj: Map[HeapNode, Map[String, Set[HeapNode]]] = objToObj,
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
    logger.debug("*** ----------------createObject(" + typ.toString + "; " + pp.toString + ")")

    ???
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
      val refToObjmap = refToObj + (x -> Set[HeapNode](NullHeapNode)) // add key to refToObj map
      // return the current state with updated exprSet and refToObj
      this.copy(exprSet = ExpressionSet(x), refToObj = refToObjmap)
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
      if (objToObj.contains(SummaryHeapNode)) { // the summary heap node exists already
        // add key to refToObj map
        val refMap = refToObj + (x -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
        // return the current state with updated refToObj and objFieldToObj
        this.copy(exprSet = ExpressionSet(x), refToObj = refMap)
      } else { // the summary heap node was never created before
        // prepare fields to add to objFieldToObj map and add variables to numDom
        var fieldMap = Map[String,Set[HeapNode]]()
        var num = numDom
        for (f <- fieldSet) { // for all fields declared within the program...
          f._1 match {
            case _:RefType =>
              fieldMap = fieldMap + (f._2 -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
            case _ =>
              num = num.createVariable(HeapAccess(SummaryHeapNode,f._2,f._1),f._1)
          }
        }
        // add key to refToObj map
        val refMap = refToObj + (x -> Set[HeapNode](SummaryHeapNode, NullHeapNode))
        // add key to objFieldToObj map
        val objMap = objToObj + (SummaryHeapNode -> fieldMap)
        // return the current state with updated exprSet, refToObj, objFieldToObj and numDom
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
  override def expr: ExpressionSet = {
    this.exprSet // return exprSet
  }

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): S = {
    val fields = Set[(Type,String)]()
    val currentPP = DummyProgramPoint
    val nonce = 1
    // return a new state with factory exprSet, empty refToObj, empty objFieldToObj, factory numDom
    val expr = ExpressionSet()
    val refToObj = Map[VariableIdentifier,Set[HeapNode]]()
    val objToObj = Map[HeapNode,Map[String,Set[HeapNode]]]()
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
    logger.debug("*** ----------------getFieldValue(" + obj.toString + "; " + field + "; " + typ.toString + ")")

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
        if (rcvSet.contains(SummaryHeapNode) && flag) { // materialization
          val freshR = HeapNode(temp); temp = temp + 1 // create fresh heap node
          rcvSet = rcvSet - SummaryHeapNode + freshR // update receiver set
          refMap = refMap + (head -> rcvSet) // add key to refMap to replace the summary node with the fresh node
          // update refMap adding the fresh node where the summary node is present
          refMap = refMap.mapValues(s => if (s.contains(SummaryHeapNode)) s + freshR else s)
          // update objMap adding the fresh node where the summary node is present
          objMap = objMap.mapValues(
            m => m.mapValues(s => if (s.contains(SummaryHeapNode)) s + freshR else s)
          )
          objMap = objMap + (freshR -> objMap(SummaryHeapNode)) // add key to objMap
          for (f <- fieldSet) { // for all fields declared within the program...
            f._1 match {
              case _:RefType =>
              case _ =>
                val sum = HeapAccess(SummaryHeapNode,f._2,f._1)
                num = num.rename(List[HeapAccess](sum),List[HeapAccess](HeapAccess(freshR,f._2,f._1)))
                num = num.createVariable(sum,f._1)
            }
          }
        }

        // path tail evaluation
        val eval = path.tail.dropRight(1).foldLeft((refMap,objMap,num,rcvSet))(
          (curr,next) => {
            var rM: Map[VariableIdentifier,Set[HeapNode]] = curr._1
            var oM: Map[HeapNode,Map[String,Set[HeapNode]]] = curr._2
            var nD: T = curr._3
            var rS = Set[HeapNode]()
            for (obj: HeapNode <- curr._4) { // for all current receivers...
              var tR = oM.getOrElse(obj,Map[String,Set[HeapNode]]()).getOrElse(next.getName,Set[HeapNode]())
              if (tR.contains(SummaryHeapNode) && flag) { // materialization
                val freshO = HeapNode(temp); temp = temp + 1 // create fresh heap node
                tR = tR - SummaryHeapNode + freshO // update receiver set
                // update rM adding the fresh node where the summary node is present
                rM = rM.mapValues(s => if (s.contains(SummaryHeapNode)) s + freshO else s)
                // update oM adding the fresh node where the summary node is present
                oM = oM.mapValues(
                  m => m.mapValues(s => if (s.contains(SummaryHeapNode)) s + freshO else s)
                )
                oM = oM + (freshO -> oM(SummaryHeapNode)) // add key to oM
                for (f <- fieldSet) { // for all fields declared within the program...
                  f._1 match {
                    case _:RefType =>
                    case _ =>
                      val sum = HeapAccess(SummaryHeapNode,f._2,f._1)
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
          for (obj: HeapNode <- eval._4 - NullHeapNode) { // for all current receivers...
            rcvSet = objMap.getOrElse(obj,Map[String,Set[HeapNode]]()).getOrElse(field,Set[HeapNode]())
            if (rcvSet.contains(SummaryHeapNode) && flag) { // materialization
              val freshE = HeapNode(temp); temp = temp + 1 // create fresh heap node
              rcvSet = rcvSet - SummaryHeapNode + freshE // update receiver set
              // update refMap adding the fresh node where the summary node is present
              refMap = refMap.mapValues(s => if (s.contains(SummaryHeapNode)) s + freshE else s)
              // update objMap adding the fresh node where the summary node is present
              objMap = objMap.mapValues(
                m => m.mapValues(s => if (s.contains(SummaryHeapNode)) s + freshE else s)
              )
              objMap = objMap + (freshE -> objMap(SummaryHeapNode)) // add key to objMap
              for (f <- fieldSet) { // for all fields declared within the program...
                f._1 match {
                  case _:RefType =>
                  case _ =>
                    val sum = HeapAccess(SummaryHeapNode,f._2,f._1)
                    num = num.rename(List[HeapAccess](sum),List[HeapAccess](HeapAccess(freshE,f._2,f._1)))
                    num = num.createVariable(sum,f._1)
                }
              }
            }
          }
        }

        // null pointer dereference warning report
        if (eval._4.contains(NullHeapNode)) Reporter.reportInfo("Possible null pointer dereference", currentPP)
        rcvSet = eval._4 - NullHeapNode
        val expr = rcvSet.foldLeft(ExpressionSet())(
          (exp,rcv) =>  exp add ExpressionSet(HeapAccess(rcv,field,typ))  // create new FieldIdentifier
        )
        // return the current state with updated exprSet, refToObj, objFieldToObj, numDom
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
    var rcvSet = refToObj.getOrElse(x,Set[HeapNode]())  // receiver set
    if (rcvSet.contains(SummaryHeapNode) && flag) { // materialization
      val freshR = HeapNode(temp); temp = temp + 1 // create fresh heap node
      rcvSet = rcvSet - SummaryHeapNode + freshR // update receiver set
      refMap = refMap + (x -> rcvSet) // add key to refMap to replace the summary node with the fresh node
      // update refMap adding the fresh node where the summary node is present
      refMap = refMap.mapValues(s => if (s.contains(SummaryHeapNode)) s + freshR else s)
      // update objMap adding the fresh node where the summary node is present
      objMap = objMap.mapValues(
        m => m.mapValues(s => if (s.contains(SummaryHeapNode)) s + freshR else s)
      )
      objMap = objMap + (freshR -> objMap(SummaryHeapNode)) // add key to objMap
      for (f <- fieldSet) { // for all fields declared within the program...
        f._1 match {
          case _:RefType =>
          case _ =>
            val sum = HeapAccess(SummaryHeapNode,f._2,f._1)
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
    logger.debug("*** lessEqual(" + this.repr + ", " + other.repr + ")")

    false //TODO
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: S): S = {
    logger.trace("*** lub(" + this.repr + ", " + other.repr + ")")
    def zipper[K](map1: Map[K,Set[HeapNode]], map2: Map[K,Set[HeapNode]]): Map[K,Set[HeapNode]] = {
      var keyToObjmap = Map[K,Set[HeapNode]]()
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,None) =>
          case (None,Some(o2)) => keyToObjmap = keyToObjmap + (key -> o2)
          case (Some(o1),None) => keyToObjmap = keyToObjmap + (key -> o1)
          case (Some(o1),Some(o2)) => // we keep the summary node only if present in both maps
            val o = (o1 - SummaryHeapNode) ++ (o2 - SummaryHeapNode) ++ (o1 & o2)
            keyToObjmap = keyToObjmap + (key -> o)
        }
      }; keyToObjmap
    }
    val fields = this.fieldSet ++ other.fieldSet  // join the fieldSets
    val allowed = this.flag || other.flag // join the materialization flags
    val temp = Math.max(this.nonce, other.nonce) // take the maximum nonce
    val expr = this.exprSet lub other.expr // join the exprSets
    val refMap = zipper[VariableIdentifier](this.refToObj,other.refToObj)  // merge the refToObjs
    // merge the objFieldToObj
    var objMap = Map[HeapNode,Map[String,Set[HeapNode]]]()
    for (key <- this.objToObj.keySet ++ other.objToObj.keySet) { // for all keys present in either map...
      (this.objToObj.get(key),other.objToObj.get(key)) match {
        case (None,None) =>
        case (None,Some(m2)) => objMap = objMap + (key -> m2)
        case (Some(m1),None) => objMap = objMap + (key -> m1)
        case (Some(m1: Map[String,Set[HeapNode]]),Some(m2: Map[String,Set[HeapNode]])) =>
          objMap = objMap + (key -> zipper[String](m1,m2))
      }
    }
    val num = this.numDom lub other.numDom // join the numDoms
    // return the current state with updated exprSet, refToObj, objFieldToObj and numDom
    this.copy(fieldSet = fields, flag = allowed, nonce = temp, exprSet = expr, refToObj = refMap, objToObj = objMap, numDom = num)
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): S = {
    // retrieve all heap nodes reachable from Ref variables
    val objFromRef = refToObj.foldLeft(Set[HeapNode]())((r, s) => r ++ s._2)
    // add all Obj reachable from fields of Obj reachable from Ref variables
    var reach = objFromRef
    for (id <- objFromRef) {
      reach = objToObj.getOrElse(id,Map[String,Set[HeapNode]]()).foldLeft(reach)((r,s) => r ++ s._2)
    }
    // collect and remove all unreachable Obj
    var unreach = Set[HeapNode]()
    var objMap = objToObj
    for (key <- objToObj.keys) {
      if (!reach.contains(key)) { objMap = objMap - key; unreach = unreach + key }
    }
    // prune variables from numDom
    var num = numDom
    for (obj <- unreach) { for (f <- fieldSet) { num = num.removeVariable(HeapAccess(obj,f._2,f._1)) } }
    // return the current state with updated objFieldToObj and numDom
    this.copy(objToObj = objMap, numDom = num)
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
    this.copy(exprSet = ExpressionSet())  // return the current state with factory exprSet
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
    val fields = Set[(Type,String)]()
    val currentPP = DummyProgramPoint
    val nonce = 0
    // return a new state with top exprSet, empty refToObj, empty objFieldToObj, top numDom
    val expr = exprSet.top()
    val refToObj = Map[VariableIdentifier,Set[HeapNode]]()
    val objToObj = Map[HeapNode,Map[String,Set[HeapNode]]]()
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
      "\tobjFieldToObj: " + objToObj.toString + "\n" +
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
    def zipper[K](map1: Map[K,Set[HeapNode]], map2: Map[K,Set[HeapNode]]): Map[K,Set[HeapNode]] = {
      var keyToObjmap = Map[K,Set[HeapNode]]()
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,None) =>
          case (None,Some(o2)) => keyToObjmap = keyToObjmap + (key -> o2)
          case (Some(o1),None) => keyToObjmap = keyToObjmap + (key -> o1)
          case (Some(o1),Some(o2)) => // we keep the summary node only if present in both maps
            val o = (o1 - SummaryHeapNode) ++ (o2 - SummaryHeapNode) ++ (o1 & o2)
            keyToObjmap = keyToObjmap + (key -> o)
        }
      }; keyToObjmap
    }
    val fields = this.fieldSet ++ other.fieldSet  // join the fieldSets
    val temp = Math.max(this.nonce, other.nonce) // take the maximum nonce
    val expr = this.exprSet widening other.expr // widen the exprSets
    val refMap = zipper[VariableIdentifier](this.refToObj,other.refToObj)  // merge the refToObjs
    // merge the objFieldToObj
    var objMap = Map[HeapNode,Map[String,Set[HeapNode]]]()
    for (key <- this.objToObj.keySet ++ other.objToObj.keySet) { // for all keys present in either map...
      (this.objToObj.get(key),other.objToObj.get(key)) match {
        case (None,None) =>
        case (None,Some(m2)) => objMap = objMap + (key -> m2)
        case (Some(m1),None) => objMap = objMap + (key -> m1)
        case (Some(m1: Map[String,Set[HeapNode]]),Some(m2: Map[String,Set[HeapNode]])) =>
          objMap = objMap + (key -> zipper[String](m1,m2))
      }
    }
    val num = this.numDom widening other.numDom // widen the numDoms
    // return the current state with updated exprSet, refToObj, objFieldToObj and numDom
    this.copy(fieldSet = fields, flag = false, nonce = temp, exprSet = expr, refToObj = refMap, objToObj = objMap, numDom = num)
  }
}

/** MayPointTo+Polyhedra Analysis State.
  *
  * @param fieldSet fields declared within the program
  * @param currentPP current program point
  * @param nonce freshly generated heap node
  * @param exprSet result of previous statement
  * @param refToObj map from Ref variables to heap objects
  * @param objToObj map from heap objects to a map from Ref fields to heap objects
  * @param numDom numerical abstract domain
  * @author Caterina Urban
  */
case class MayPointToPolyhedraState(fieldSet: Set[(Type, String)],
                                    currentPP: ProgramPoint,
                                    flag: Boolean,
                                    nonce: Int,
                                    exprSet: ExpressionSet,
                                    refToObj: Map[VariableIdentifier, Set[HeapNode]],
                                    objToObj: Map[HeapNode, Map[String, Set[HeapNode]]],
                                    numDom: Apron.Polyhedra)
  extends MayPointToNumericalState[Apron.Polyhedra,MayPointToPolyhedraState] {
  override def copy(fieldSet: Set[(Type, String)],
                    currentPP: ProgramPoint,
                    flag: Boolean,
                    nonce: Int,
                    exprSet: ExpressionSet,
                    refToObj: Map[VariableIdentifier, Set[HeapNode]],
                    objToObj: Map[HeapNode, Map[String, Set[HeapNode]]],
                    numDom: Polyhedra): MayPointToPolyhedraState =
    MayPointToPolyhedraState(fieldSet, currentPP, flag, nonce, exprSet, refToObj, objToObj, numDom)
}

/** MayPointTo+Numerical Analysis Entry State.
  *
  * @tparam T the numerical domain
  * @tparam S the maypointto+numerical state
  * @author Caterina Urban
  */
trait MayPointToNumericalEntryStateBuilder[T <: NumericalDomain[T], S <: MayPointToNumericalState[T,S]] extends EntryStateBuilder[S] {

  protected var fields: Set[(Type,String)] = Set[(Type,String)]()

  override def build(method: MethodDeclaration): S = {
    fields = Set[(Type,String)]()
    for(f <- method.classDef.fields) {
      fields = fields + ((f.typ, f.variable.toString))
    }
    method.initializeArgument[S](topState.copy(fieldSet = fields))
  }

}

/** MayPointTo+Polyhedra Analysis Entry States.
  *
  * @author Caterina Urban
  */
object MayPointToPolyhedraEntryStateBuilder
  extends MayPointToNumericalEntryStateBuilder[Apron.Polyhedra, MayPointToPolyhedraState] {

  override def topState = MayPointToPolyhedraState(fields, DummyProgramPoint, true, 1,
    ExpressionSet(),
    Map[VariableIdentifier,Set[HeapNode]](),
    Map[HeapNode,Map[String,Set[HeapNode]]](),
    Apron.Polyhedra.Bottom.factory)
}

/** MayPointTo+Numerical Analysis Runner.
  *
  * @tparam N the numerical domain
  * @tparam T the maypointto+numerical state
  * @author Caterina Urban
  */
trait MayPointToNumericalAnalysisRunner[N <: NumericalDomain[N], T <: MayPointToNumericalState[N,T]] extends SilAnalysisRunner[T] {

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

/** MayPointTo+Polyhedra Analysis Runner.
  *
  * @author Caterina Urban
  */
object MayPointToPolyhedraAnalysisRunner
  extends MayPointToNumericalAnalysisRunner[Apron.Polyhedra, MayPointToPolyhedraState] {
  override val analysis = SimpleAnalysis[MayPointToPolyhedraState](MayPointToPolyhedraEntryStateBuilder)
  override def toString = "MayPointTo+Polyhedra Analysis"
}
