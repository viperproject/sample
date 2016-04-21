/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.{File, PrintWriter}

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{Apron, BoxedNonRelationalNumericalDomain, DoubleInterval, NumericalDomain}
import breeze.optimize.linear._
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.execution.{Analysis}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSilConverter, SilAnalysisRunner, sample}
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, MethodDeclaration, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging
import viper.silicon.Silicon
import viper.silver.{ast => sil}
import viper.silver.ast.SourcePosition

/** Symbolic Access.
  *
  * @param path the list of identifiers forming the (reverse) path
  * @author Caterina Urban
  */
case class SymbolicAccess(path: List[Identifier]) {
  override def equals(a: Any): Boolean = a match {
    case x: SymbolicAccess => this.path equals x.path
    case _ => false
  }
  override def toString : String = "acc(" + this.path.reverse.map(_.getName).mkString(".") + ")"
}

/** Counted Symbolic Access.
  *
  * It is either a monomial `n * s` or a constant `n` (with `null` symbolic value).
  * Monomials are used to refer within a program to occurrences of access permissions in pre-conditions.
  * Constants are used to express the access permissions available within a program.
  *
  * @param n the number of times the symbolic value is taken into account
  * @param s symbolic value taken into account
  * @author Caterina Urban
  */
case class CountedAccess(n: Double, s: SymbolicAccess) {

  def this(n: Double) = this(n, null)

  override def equals(a: Any): Boolean = a match {
    case x: CountedAccess => (this.n equals x.n) && this.sameSymbolicAccess(x)
    case _ => false
  }
  def sameSymbolicAccess(a: CountedAccess): Boolean = {
    if (this.s == null && a.s == null) return true
    if (this.s == null || a.s == null) return false
    this.s equals a.s
  }

  def evaluate(result: Map[SymbolicAccess, Double]): Double = n * result.getOrElse(s,0.0)

  def glb(b : CountedAccess) = {
    assert(this.sameSymbolicAccess(b)); CountedAccess(Math.max(this.n, b.n), this.s)
  }
  def lub(b : CountedAccess) = {
    assert(this.sameSymbolicAccess(b)); CountedAccess(Math.min(this.n, b.n), this.s)
  }

  def +(b: CountedAccess) = {
    assert(this.sameSymbolicAccess(b)); CountedAccess(this.n+b.n, this.s)
  }
  def -(b: CountedAccess) = {
    assert(this.sameSymbolicAccess(b)); CountedAccess(this.n-b.n, this.s)
  }

  override def toString = s match {
    case null => this.n.toString
    case k => this.n.toString + "*" + this.s.toString
  }
}

/** Symbolic Access Permission.
  *
  * @author Caterina Urban
  */
class SymbolicPermission {

  def this(v: CountedAccess) = { this(); this.value = if (v.n != 0) this.value + v else this.value }
  def this(s: Set[CountedAccess]) = { this(); this.value = s.filter((el) => el.n != 0) }
  
  var value: Set[CountedAccess] = Set[CountedAccess]()

  def evaluate(result: Map[SymbolicAccess, Double]): Double = value.foldLeft(0.0)(_ + _.evaluate(result))
  def extend(field: Identifier): SymbolicPermission = new SymbolicPermission(this.value.foldLeft(Set[CountedAccess]())(
    (set, c: CountedAccess) => set + CountedAccess(c.n, SymbolicAccess(field :: c.s.path))
  ))

  def glb(other: SymbolicPermission): SymbolicPermission = {
    val sym = this.value intersect other.value.map(
      (s) => this.value.foldLeft(s)((el, v) => if (s sameSymbolicAccess v) el glb v else el)
    )
    new SymbolicPermission(sym)
  }
  def lub(other: SymbolicPermission): SymbolicPermission = {
    val sym = this.value union other.value.map(
      (s) => this.value.foldLeft(s)((el, v) => if (s sameSymbolicAccess v) el lub v else el)
    )
    new SymbolicPermission(sym)
  }

  private def addCountedAccess(s: Set[CountedAccess], v: CountedAccess): Set[CountedAccess] = {
    var added = false
    val sym: Set[CountedAccess] = s.map(
      // adding the counted symbolic value with an existing element with the same symbolic value
      (el) => if (el.sameSymbolicAccess(v)) { added = true; el + v } else el
    )
    // adding the counted symbolic value in case there is no element with the same symbolic value
    if (!added && v.n != 0) sym + v else sym
  }
  def +(v: CountedAccess): SymbolicPermission = {
    val sym: Set[CountedAccess] = addCountedAccess(this.value,v); new SymbolicPermission(sym)
  }
  def ++(v: SymbolicPermission): SymbolicPermission = {
    val sym: Set[CountedAccess] = v.value.foldLeft(this.value)(addCountedAccess); new SymbolicPermission(sym)
  }
  
  private def subCountedAccess(s: Set[CountedAccess], v: CountedAccess): Set[CountedAccess] = {
    var subtracted = false
    val sym: Set[CountedAccess] = s.map(
      // subtracting the counted symbolic value from an existing element with the same symbolic value
      (s) => if (s.sameSymbolicAccess(v)) { subtracted = true; s - v } else s
    )
    // adding the negated counted symbolic value in case there is no element with the same symbolic value
    if (!subtracted && v.n != 0) sym + CountedAccess(-v.n,v.s) else sym
  }
  def -(v : CountedAccess) : SymbolicPermission= {
    val sym: Set[CountedAccess] = subCountedAccess(this.value,v); new SymbolicPermission(sym)
  }
  def --(v: SymbolicPermission): SymbolicPermission = {
    val sym: Set[CountedAccess] = v.value.foldLeft(this.value)(subCountedAccess); new SymbolicPermission(sym)
  }

  override def toString : String = if (value.isEmpty) "0" else value.mkString("", " + ", "")
}

/** Access Permission State.
  *
  * @tparam N the numerical domain
  * @tparam T the maypointto+numerical state
  * @tparam S the access permission state
  * @author Caterina Urban
  */
trait AccessPermissionState[N <: NumericalDomain[N], T <: MayPointToNumericalState[N,T], S <: AccessPermissionState[N,T,S]]
  extends SimplePermissionState[S] with StateWithBackwardAnalysisStubs[S] with LazyLogging
{
  this: S =>

  def heapNum: T // may pointto+numerical state
  // map from heap nodes to their associated set of symbolic permissions
  def nodeToSym: Map[HeapNode, Set[SymbolicPermission]]

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
    logger.debug("*** assignField(" + obj.toString + "; " + field.toString + "; " + right.toString + ")")

    obj match {
      case obj: HeapAccess =>
        val heap = heapNum.assignField(obj, field, right)
        var nodeMap = nodeToSym
        // ensure write access permission
        PermissionSolver.ensureWrite(nodeToSym(obj.rcv),VariableIdentifier(obj.field)(obj.typ))
        // ensure read access permissions for all accesses in the right-hand side
        for (id <- right.ids.getNonTop) {
          id match {
            case id: HeapAccess =>
              val fId: Identifier = VariableIdentifier(id.field)(id.typ)
              PermissionSolver.ensureRead(nodeToSym(id.rcv),fId) // ensure read access permission
              // add key to nodeMap to represent the access
              val rgtSet = heapNum.objToObj.getOrElse(id.rcv,Map[String,Set[HeapNode]]()).getOrElse(id.field,Set[HeapNode]())
              for (o <- rgtSet) {
                if (!nodeMap.contains(o)) {
                  nodeMap = nodeMap + (o -> nodeMap(id.rcv).map(_.extend(fId)))
                }
              }
            case _ => // nothing to be done
          }
        }
        if (obj.typ.isObject) { // the assigned field is a Ref
          right match {
            case right: HeapAccess => // e.g., `x.f := y.g`

              // return the current state with updated heapNum, nodeToSym
              this.copy(heapNum = heap, nodeToSym = nodeMap).pruneUnreachableHeap()

            case right: Constant => // e.g., `x.f := null`

              // return the current state with updated heapNum, nodeToSym
              this.copy(heapNum = heap, nodeToSym = nodeMap).pruneUnreachableHeap()

            case right: VariableIdentifier => // e.g., `x.f := y`

              // return the current state with updated heapNum, nodeToSym
              this.copy(heapNum = heap, nodeToSym = nodeMap).pruneUnreachableHeap()

            case _ => throw new NotImplementedError("A field assignment implementation is missing.")
          }
        } else {  // the assigned field is not a Ref
          // return the current state with updated heapNum
          this.copy(heapNum = heap).pruneUnreachableHeap()
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
    logger.debug("*** assignVariable(" + x.toString + "; " + right.toString + ")")

    val heap = heapNum.assignVariable(x, right)
    var nodeMap = nodeToSym
    // ensure read access permissions for all accesses in the right-hand side
    for (id <- right.ids.getNonTop) {
      id match {
        case id: HeapAccess =>
          val fId: Identifier = VariableIdentifier(id.field)(id.typ)
          PermissionSolver.ensureRead(nodeToSym(id.rcv),fId) // ensure read access permission
          // add key to nodeMap to represent the access
          val rgtSet = heapNum.objToObj.getOrElse(id.rcv,Map[String,Set[HeapNode]]()).getOrElse(id.field,Set[HeapNode]())
          for (o <- rgtSet) {
            if (!nodeMap.contains(o)) {
              nodeMap = nodeMap + (o -> nodeMap(id.rcv).map(_.extend(fId)))
            }
          }
        case _ => // nothing to be done
      }
    }
    // return the current state with updated heapNum, nodeToSym
    this.copy(heapNum = heap, nodeToSym = nodeMap)
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): S = {
    logger.debug("*** assume(" + cond.toString + ")")

    val heap = heapNum.assume(cond)
    var nodeMap = nodeToSym
    // ensure read access permissions for all accesses in the expression
    for (id <- cond.ids.getNonTop) {
      id match {
        case id: HeapAccess =>
          val fId: Identifier = VariableIdentifier(id.field)(id.typ)
          PermissionSolver.ensureRead(nodeToSym(id.rcv),fId) // ensure read access permission
        // add key to nodeMap to represent the access
        val rgtSet = heapNum.objToObj.getOrElse(id.rcv,Map[String,Set[HeapNode]]()).getOrElse(id.field,Set[HeapNode]())
          for (o <- rgtSet) {
            if (!nodeMap.contains(o)) {
              nodeMap = nodeMap + (o -> nodeMap(id.rcv).map(_.extend(fId)))
            }
          }
        case _ => // nothing to be done
      }
    }
    // return the current state with updated heapNum
    this.copy(heapNum = heap)
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): S = {
    logger.debug("\n*** before(" + pp.toString + "): " + this.repr)
    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.before(pp))
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): S = this.copy(heapNum.bottom(), Map[HeapNode, Set[SymbolicPermission]]())

  def copy(heapNum: T = heapNum, nodeToSym: Map[HeapNode,Set[SymbolicPermission]] = nodeToSym): S

  /** Creates an object at allocation site.
    *
    * Invoked by calls to `new()`.
    *
    * @param typ The dynamic type of the created object
    * @param pp The allocation site of the object
    * @return The abstract state with updated exprSet after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): S = {
    logger.debug("*** createObject(" + typ.toString + "; " + pp.toString + ")")

//    //    val obj = HeapIdentifier(typ,pp) // create new Obj
//    //    val objFieldToObjmap = objFieldToObj + (obj -> Map[String,Set[HeapIdentifier]]()) // add key to objFieldToObj map
//    //    // return the current state with updated exprSet and updated objFieldToObj map
//    //    this.copy(exprSet = ExpressionSet(obj), objFieldToObj = objFieldToObjmap)
//
//    this.copy(heapNum = heapNum.createObject(typ, pp))
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
    logger.debug("*** createVariable(" + x.toString + "; " + typ.toString + "; " + pp.toString + ")")
    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.createVariable(x, typ, pp))
  }

  /** Creates a variable for a method argument.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): S = {
    logger.debug("*** createVariableForArgument(" + x.toString + "; " + typ.toString + ")")

//    if (typ.isObject) { // the variable to be created is a `Ref`
//    val obj = HeapIdentifier(typ, x.pp) // create new heap object
//    val sumObj = HeapIdentifier(typ, DummyProgramPoint).setSummary(true) // create new summary heap object
//    var idToSymmap = idToSym // update idToSym map...
//      for (f <- heapNum.fieldSet) { // for each field declared in the program...
//      // create a field identifier for the heap object
//      val fld = FieldIdentifier(obj,f._2,f._1)
//        // create a field identifier for the summary heap object
//        val sumFld = FieldIdentifier(sumObj,f._2,f._1)
//        // create new symbolic access precondition for the path formed by the variable being created and the current field
//        val pre = SymbolicPrecondition(new Path(List(f._2, x.name), List(f._1, x.typ), List(x.pp, x.pp)))
//        val sym = new SymbolicAccessPermission(CountedSymbolicAccess(1,pre))
//        // add keys to idToSym map
//        idToSymmap = idToSymmap + (fld -> Set[SymbolicAccessPermission](sym))
//        // note that the field identifier corresponding to the summary heap object could already be in the idToSym map
//        idToSymmap = idToSymmap + (sumFld -> (idToSymmap.getOrElse(sumFld,Set[SymbolicAccessPermission]()) + sym))
//      }
//      // return the current state with updated heapNum and updated idToSym
//      this.copy(heapNum = heapNum.createVariableForArgument(x, typ), idToSym = idToSymmap)
//    } else { // the variable to be created is not a `Ref`
//      // return the current state with updated heapNum
//      this.copy(heapNum = heapNum.createVariableForArgument(x, typ))
//    }
    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.createVariableForArgument(x, typ))
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
    // return the current state with updated heapNum
    this.copy(heapNum = heapNum.evalConstant(value, typ, pp))
  }

  /** Exhales permissions. */
  override def exhale(acc: Expression) : S = {
    logger.debug("*** exhale(" + acc.toString + "): implement me!")

//    //acc match {
//    //  case acc: PermissionExpression => //TODO: handle permission levels different than the full permission level
//    //    acc.id match {
//    //      case id: FieldIdentifier =>
//    //        // retrieve the symbolic permission associated with the permission expression
//    //        val sym: SymbolicAccessPermission = idToSym()
//    //        // create a counted symbolic value to add to the symbolic permission
//    //        val v: CountedSymbolicAccess = new CountedSymbolicAccess(acc.p.toString.toDouble)
//    //        // update key in idToSym map
//    //        val idToSymmap = idToSym + (id -> (sym - v))
//    //
//    //        // create constraint to ensure proper exhale
//    //        // val c = new Geq(PermissionSolver.convertSymbolicPermission(sym), PermissionSolver.convertCountedSymbolicValue(v))
//    //        // add constraint to solver
//    //        // PermissionSolver.addConstraint(c)
//    //
//    //        // return the current state with updated idToSym
//    //        this.copy(idToSym = idToSymmap)
//    //      case _ => throw new IllegalArgumentException("A permission exhale must occur via a FieldIdentifier")
//    //    }
//    //  case _ =>
//    //    val asserted: S = this.setExpression(ExpressionSet(acc))
//    //    assert(asserted.testFalse() lessEqual this.bottom())
//    //    this.assume(acc)
//    //}
//    this
    ???
  }

  /** The current expression.
    *
    * Invoked after each statement to retrieve its result.
    */
  override def expr: ExpressionSet = heapNum.expr

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): S = this.copy(heapNum.factory(), Map[HeapNode, Set[SymbolicPermission]]())

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
    logger.debug("*** getFieldValue(" + obj.toString + "; " + field + "; " + typ.toString + ")")

    obj match {
      case obj:AccessPathIdentifier =>
        val heap = heapNum.getFieldValue(obj, field, typ) // update heapNum

        val path: List[Identifier] = obj.path // path to evaluate
        // path head evaluation
        val partial = List[Identifier](path.head) // initial partial path
        var nodeMap = nodeToSym // initial nodeMap
        val rcvSet = heap.refToObj(path.head.asInstanceOf[VariableIdentifier]) - NullHeapNode // initial receiver set
        val initSet: Set[(HeapNode,List[Identifier])] = rcvSet.map(h => (h,partial)) // initial receiver+paths set
        // path tail evaluation
        val eval = path.tail.dropRight(1).foldLeft(nodeMap,initSet)(
          (curr,next) => {
            var nM: Map[HeapNode, Set[SymbolicPermission]] = curr._1
            var iS = Set[(HeapNode,List[Identifier])]() // initially empty next receiver set
            for ((o,pP) <- curr._2) { // for all current receivers...
              // add key to nM to represent the access
              if (!nM.contains(o)) {
                val sym = new SymbolicPermission(CountedAccess(1,SymbolicAccess(pP)))
                nM = nM + (o -> Set[SymbolicPermission](sym))
              }
              PermissionSolver.ensureRead(nM(o),next) // ensure read access permission
              val rS = heap.objToObj.getOrElse(o,Map[String,Set[HeapNode]]()).getOrElse(next.getName,Set[HeapNode]()) - NullHeapNode
              // update the next receiver+path set
              iS = iS ++ rS.map(h => (h, next::pP)) // extend the partial path
            }
            (nM, iS)
          }
        )
        nodeMap = eval._1
        for ((o,p) <- eval._2) { // for all final receivers...
          if (!nodeMap.contains(o)) {
            val sym = new SymbolicPermission(CountedAccess(1,SymbolicAccess(p)))
            nodeMap = nodeMap + (o -> Set[SymbolicPermission](sym))
          }
        } // add key to nodeMap to represent the access
        // return the current state with updated heapNum and nodeToSym
        this.copy(heapNum = heap, nodeToSym = nodeMap)
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
    logger.debug("*** getVariableValue(" + id.toString + ")")

    val heap = heapNum.getVariableValue(id)
    val pP = List[Identifier](id) // initial partial path
    var nodeMap = nodeToSym // initial nodeMap
    val rcvSet = heap.refToObj.getOrElse(id.asInstanceOf[VariableIdentifier],Set[HeapNode]()) // receiver set
    for (o <- rcvSet - NullHeapNode) { // for all receivers...
      // add key to nM to represent the access
      if (!nodeMap.contains(o)) {
        val sym = new SymbolicPermission(CountedAccess(1,SymbolicAccess(pP)))
        nodeMap = nodeMap + (o -> Set[SymbolicPermission](sym))
      }
    }
    // return the current state with updated heapNum, nodeToSym
    this.copy(heapNum = heap, nodeToSym = nodeMap)
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: S): S = {
    logger.debug("*** glb(" + other.repr + "): implement me!")

    def zipper[K](map1: Map[HeapNode,Set[SymbolicPermission]], map2: Map[HeapNode,Set[SymbolicPermission]]) = {
      var nodeMap = Map[HeapNode,Set[SymbolicPermission]]()
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,_) =>
          case (_,None) =>
          case (Some(o1),Some(o2)) => nodeMap = nodeMap + (key -> (o1 & o2))
        }
      }; nodeMap
    }
    val nodeMap = zipper[VariableIdentifier](this.nodeToSym,other.nodeToSym)  // merge the nodeToSyms
    // return the current state with updated heapNum and nodeToSym
    this.copy(heapNum = this.heapNum glb other.heapNum, nodeToSym = nodeMap)
  }

  /** Inhales permissions. */
  override def inhale(acc: Expression) : S = {
    logger.debug("*** inahle(" + acc.toString + ")")

//    acc match {
//      case acc: PermissionExpression => //TODO: handle permission levels different than the full permission level
//        acc.id match {
//          case id: FieldIdentifier =>
//          //println("INHALE: " + idToSym(id))
//          //        if (id.obj.representsSingleVariable) {
//          //          // retrieve the symbolic permission associated with the permission expression
//          //          val sym: SymbolicAccessPermission = idToSym()
//          //          // create a counted symbolic value to add to the symbolic permission
//          //          val v: CountedSymbolicAccess = new CountedSymbolicAccess(acc.p.toString.toDouble)
//          //          // update key in idToSym map
//          //          val idToSymmap = idToSym + (id -> (sym + v))
//          //
//          //          // create constraint to ensure proper inhale
//          //          // val m = new CountedSymbolicAccess(PermissionSolver.permissionType.maxLevel)
//          //          // val c = new Geq(PermissionSolver.convertCountedSymbolicValue(m), PermissionSolver.convertSymbolicPermission(sym + v))
//          //          // add constraint to solver
//          //          // PermissionSolver.addConstraint(c)
//          //
//          //          // return the current state with updated idToSym
//          //          this.copy(idToSym = idToSymmap)
//          //        } else { this } // no inhale on summary nodes
//
//          //// add constraints to the solver
//          //for (sym <- idToPerm(obj)) { // for all currently associated symbolic permissions...
//          //// create constraint to ensure write permissions
//          //val c = PermissionSolver.permissionType.ensureWrite(PermissionSolver.convertSymbolicPermission(sym))
//          //  // add constraint to solver
//          //  PermissionSolver.addConstraint(c)3
//          //}
//
//          case _ => throw new IllegalArgumentException("A permission inhale must occur via a FieldIdentifier")
//        }
//      case _ => this.assume(acc)
//    }
//    this
    ???
  }

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return `true` if and only if the state is equivalent to bottom
    */
  override def isBottom: Boolean = heapNum.isBottom

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return `true` if and only if the state is equivalent to top
    */
  override def isTop: Boolean = heapNum.isTop

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return `true` if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: S): Boolean = {
    logger.debug("*** lessEqual(" + other.repr + ")")

    val heap = this.heapNum lessEqual other.heapNum
    var nodeMap = true
    for (key <- this.nodeToSym.keySet ++ other.nodeToSym.keySet) { // for all keys present in either map...
      (this.nodeToSym.get(key),other.nodeToSym.get(key)) match {
        case (None,_) =>
        case (_,None) => nodeMap = false
        case (Some(o1),Some(o2)) => nodeMap = (o1 equals o2) && nodeMap
      }
    }
    heap && nodeMap
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: S): S = {
    logger.debug("*** lub(" + other.repr + ")")

    def zipper[K](map1: Map[HeapNode,Set[SymbolicPermission]], map2: Map[HeapNode,Set[SymbolicPermission]]) = {
      var nodeMap = Map[HeapNode,Set[SymbolicPermission]]()
      for (key <- map1.keySet ++ map2.keySet) { // for all keys present in either map...
        (map1.get(key),map2.get(key)) match {
          case (None,None) =>
          case (None,Some(o2)) => nodeMap = nodeMap + (key -> o2)
          case (Some(o1),None) => nodeMap = nodeMap + (key -> o1)
          case (Some(o1),Some(o2)) => nodeMap = nodeMap + (key -> (o1 ++ o2))
        }
      }; nodeMap
    }
    val nodeMap = zipper[VariableIdentifier](this.nodeToSym,other.nodeToSym)  // merge the nodeToSyms
    // return the current state with updated heapNum and nodeToSym
    this.copy(heapNum = this.heapNum lub other.heapNum, nodeToSym = nodeMap)
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): S = {
//    // logger.debug("*** pruneUnreachableHeap()")
//    this.copy(heapNum = heapNum.pruneUnreachableHeap())
    this
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
  override def removeExpression(): S = this.copy(heapNum = heapNum.removeExpression())

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
  private def repr: String = {
    "PermissionState(" +
      "PointsToNumericalState(" +
      heapNum.exprSet.toString + ", " +
      heapNum.refToObj.toString + ", " +
      heapNum.objToObj.toString + ", " +
      heapNum.numDom.toString + "), " +
      nodeToSym.toString + ")"
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
  override def setExpression(expr: ExpressionSet): S = this.copy(heapNum = heapNum.setExpression(expr))

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
  override def top(): S = this.copy(heapNum.top(), Map[HeapNode, Set[SymbolicPermission]]())

  /** The state string representation.
    *
    * @return the string representation of the current state
    */
  override def toString: String =
    "PermissionState(\n" +
      "\theapNum: " + heapNum.repr + "\n" +
      "\tnodeToSym: " + nodeToSym.toString + "\n" +
      ")"

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: S): S = {
    logger.debug("*** widening(" + other.repr + ")")

//    val idToSymmap = this.idToSym.filterKeys(k => !other.idToSym.contains(k)) ++ other.idToSym.map {
//      case (k: FieldIdentifier,v: Set[SymbolicAccessPermission]) => k -> (v ++ this.idToSym.getOrElse(k,Set[SymbolicAccessPermission]()))
//    } // merge the itToSyms
//    val idToPermmap = this.idToPerm.filterKeys(k => !other.idToPerm.contains(k)) ++ other.idToPerm.map {
//        case (k: FieldIdentifier,v: Set[SymbolicAccessPermission]) => k -> (v ++ this.idToPerm.getOrElse(k,Set[SymbolicAccessPermission]()))
//      } // merge the itToPerm
//
//    // return the current state with updated heapNum, updated idToSym and updated idToPerm
//    this.copy(heapNum = heapNum widening other.heapNum, idToSym = idToSymmap, idToPerm = idToPermmap)
    ???
  }
}

sealed trait PermissionConstraint
case class EqP(left: SymbolicPermission, right: SymbolicPermission) extends PermissionConstraint {
  override def toString = left.toString + "=" + right.toString
}
case class GtP(left: SymbolicPermission, right: SymbolicPermission) extends PermissionConstraint {
  override def toString = left.toString + ">" + right.toString
}

/** Access Permission Constraints Solver.
  *
  * @author Caterina Urban
  */
object PermissionSolver {

  /** Minumum permission value. */
  private def minLevel: Double = 0
  /** Epsilon permission value. */
  private val epsilon : Double = 0.1
  /** Maximum permission value. */
  private def maxLevel: Double = 1

  /** Set of constraints to be solved. */
  private var constraints: Set[PermissionConstraint] = Set[PermissionConstraint]()
  /** Gets the current set of constraints. */
  def getConstraints : Set[PermissionConstraint] = constraints
  /** Ensures write permissions. */
  def ensureWrite(level: Set[SymbolicPermission], field: Identifier) =
    for (p <- level) {
      constraints = constraints + new EqP(p.extend(field), new SymbolicPermission(new CountedAccess(maxLevel)))
    }
  /** Ensures read permissions. */
  def ensureRead(level: Set[SymbolicPermission], field: Identifier) =
    for (p <- level) {
      constraints = constraints + new GtP(p.extend(field), new SymbolicPermission(new CountedAccess(minLevel)))
    }
  /** Empties the current set of constraints. */
  def emptyConstraints() = { constraints = Set[PermissionConstraint]() }

  /** Collects the symbolic accesses within a set of constraints. */
  private def getSymbolicAccess(set: Set[PermissionConstraint]): Set[SymbolicAccess] =
    set.foldLeft(Set[SymbolicAccess]())(_ ++ getSymbolicAccess(_))
  private def getSymbolicAccess(c: PermissionConstraint): Set[SymbolicAccess] = c match {
    case EqP(left, right) => getSymbolicAccess(left) ++ getSymbolicAccess(right)
    case GtP(left, right) => getSymbolicAccess(left) ++ getSymbolicAccess(right)
  }
  private def getSymbolicAccess(p: SymbolicPermission): Set[SymbolicAccess] =
    p.value.foldLeft(Set[SymbolicAccess]())((set, c) => if (c.s != null) set + c.s else set)

  /** Converts a constraint into a LinearProgram#Constraint (accepted by Breeze) */
  private def toConstraint(lp: LinearProgram)(z: lp.Expression, vars: Map[SymbolicAccess,lp.Variable], c: PermissionConstraint): Set[lp.Constraint] = c match {
    case EqP(left, right) =>
      val l = toExpression(lp)(z, vars, left)
      val r = toExpression(lp)(z, vars, right)
      Set[lp.Constraint]((l >= r).asInstanceOf[lp.Constraint], (l <= r).asInstanceOf[lp.Constraint])
    case GtP(left, right) =>
      val l = toExpression(lp)(z, vars, left)
      val r = toExpression(lp)(z, vars, right)
      Set[lp.Constraint]((l - r >= epsilon).asInstanceOf[lp.Constraint])
  }
  private def toExpression(lp: LinearProgram)(z: lp.Expression, vars: Map[SymbolicAccess,lp.Variable], p: SymbolicPermission): lp.Expression =
    p.value.foldLeft(z: lp.Expression)(
      (exp, v) => if (v.s == null) exp + v.n else exp + vars(v.s) * v.n
    )

  /** Solves a set of constraints. */
  def solve(constraints: Set[PermissionConstraint]): Map[SymbolicAccess,Double] = {

    val lp = new LinearProgram() // linear program
    val zero = lp.Real("") // special zero variable
    var result = Map[SymbolicAccess, Double]() // result

    // array of symbolic values
    val sym: Array[SymbolicAccess] = this.getSymbolicAccess(constraints).toArray
    // map from symbolic values to corresponding LinearProgram#Variable (accepted by Breeze)
    val symToVars: Map[SymbolicAccess,lp.Variable] = sym.foldLeft(Map[SymbolicAccess,lp.Variable]())(
      (m, s) => m + (s -> lp.Real(s.toString))
    )

    val vars = symToVars.values.toList // list of values corresponding to symbolic variables
    if (vars.length > 0) {
      // objective function: the sum of the variables
      val obj = vars.drop(1).foldLeft(vars.head: lp.Expression)((exp: lp.Expression, v: lp.Variable) => exp + v)

      var prob = obj.subjectTo() // lp problem
      prob = prob.subjectTo(zero >= 0).subjectTo(zero <= 0) // adding special constraint on special zero variable
      for (v <- vars) { prob = prob.subjectTo(v >= 0).subjectTo(v <= 1) } // adding variable bounds
      for (c <- constraints) { // adding constraints
        for (s <- toConstraint(lp)(zero, symToVars, c)) { prob = prob.subjectTo(s.asInstanceOf[lp.Constraint]) }
      }
      val res = lp.minimize(prob).result // solving lp problem

      // computing the result (ignoring the special zero variable)
      for (i <- 1 to sym.length) { result = result + (sym(i - 1) -> res.valueAt(i)) }; result
    } else result
  }

  /** Converts a double to a rational number representation. */
  def doubleToRational(d: Double): (Int, Int) = {

    def gcd(a: Int,b: Int): Int = if (b == 0) a else gcd(b, a % b)

    val s = d.toString
    val n = s.length - 1 - s.indexOf('.')
    var r = d
    var den = 1
    for (i <- 1 to n) { r = r * 10; den = den * 10 }
    var num = Math.round(r).toInt

    var g = gcd(num,den)
    while (g != 1) { num = num / g; den = den / g; g = gcd(num,den) }

    (num, den)
  }

}

/** Access Permission State using Intervals.
  *
  * @param heapNum maypointto+intervals analysis state
  * @param nodeToSym map from heap nodes to symbolic permissions
  * @author Caterina Urban
  */
case class AccessIntervalsState(heapNum: MayPointToIntervalsState, nodeToSym: Map[HeapNode,Set[SymbolicPermission]])
  extends AccessPermissionState[NumDom.I,MayPointToIntervalsState,AccessIntervalsState] {
  override def copy(heapNum: MayPointToIntervalsState,
                    nodeToSym: Map[HeapNode, Set[SymbolicPermission]]): AccessIntervalsState =
    AccessIntervalsState(heapNum, nodeToSym)
}

/** Access Permission State using Polyhedra.
  *
  * @param heapNum maypointto+polyhedra analysis state
  * @param nodeToSym map from heap nodes to symbolic permissions
  * @author Caterina Urban
  */
case class AccessPolyhedraState(heapNum: MayPointToPolyhedraState, nodeToSym: Map[HeapNode,Set[SymbolicPermission]])
  extends AccessPermissionState[NumDom.P,MayPointToPolyhedraState,AccessPolyhedraState] {
  override def copy(heapNum: MayPointToPolyhedraState,
                    nodeToSym: Map[HeapNode, Set[SymbolicPermission]]): AccessPolyhedraState =
    AccessPolyhedraState(heapNum, nodeToSym)
}

/** Access Permission Inference Entry State.
  *
  * @tparam N the numerical domain
  * @tparam T the maypointto+numerical state
  * @tparam S the access permission state
  * @author Caterina Urban
  */
trait AccessPermissionEntryStateBuilder[N <: NumericalDomain[N], T <: MayPointToNumericalState[N,T],
  S <: AccessPermissionState[N,T,S]] extends EntryStateBuilder[S] {

  protected var fields: Set[(Type,String)] = Set[(Type,String)]()

  override def build(method: MethodDeclaration): S = {
    fields = Set[(Type,String)]()
    for(f <- method.classDef.fields) {
      fields = fields + ((f.typ, f.variable.toString))
    }
    method.initializeArgument[S](topState.copy(heapNum = topState.heapNum.copy(fieldSet = fields)))
  }

}

/** Access Permission Inference Entry State using Intervals.
  *
  * @author Caterina Urban
  */
object AccessIntervalsEntryStateBuilder
  extends AccessPermissionEntryStateBuilder[NumDom.I,MayPointToIntervalsState,AccessIntervalsState] {
  override def topState: AccessIntervalsState = AccessIntervalsState(
    MayPointToIntervalsEntryStateBuilder.topState,
    Map[HeapNode,Set[SymbolicPermission]]())
}

/** Access Permission Inference Entry State using Polyhedra.
  *
  * @author Caterina Urban
  */
object AccessPolyhedraEntryStateBuilder
  extends AccessPermissionEntryStateBuilder[NumDom.P,MayPointToPolyhedraState,AccessPolyhedraState] {
  override def topState: AccessPolyhedraState = AccessPolyhedraState(
    MayPointToPolyhedraEntryStateBuilder.topState,
    Map[HeapNode,Set[SymbolicPermission]]())
}

/** Access Permission Inference.
  *
  * @tparam N the numerical domain
  * @tparam T the maypointto+numerical state
  * @tparam S the access permission state
  * @author Caterina Urban
  */
trait AccessPermissionInference[N <: NumericalDomain[N], T <: MayPointToNumericalState[N,T], S <: AccessPermissionState[N,T,S]]
  extends Analysis[S] {

  // map between method names and constraint solutions
  var permissions = Map[String, Map[SymbolicAccess,Double]]()

  def entryStateBuilder: AccessPermissionEntryStateBuilder[N,T,S]

  override def analyze(method: MethodDeclaration): AnalysisResult[S] = {
    val result = analyze(method, entryStateBuilder.build(method)) // analyze the method
    // solve the accumulated constraints
    val solution = PermissionSolver.solve(PermissionSolver.getConstraints)
    // add entry to the map between method names and constraint solutions
    permissions = permissions + (method.name.toString -> solution)
    // clear the accumulated constraints (for the analysis of the next method)
    PermissionSolver.emptyConstraints()
    result // return the result of the analysis
  }

}

/** Access Permission Inference using Intervals.
  *
  * @author Caterina Urban
  */
object AccessIntervalsInference
  extends AccessPermissionInference[NumDom.I,MayPointToIntervalsState, AccessIntervalsState] {
  override def entryStateBuilder = AccessIntervalsEntryStateBuilder
}

/** Access Permission Inference using Polyhedra.
  *
  * @author Caterina Urban
  */
object AccessPolyhedraInference
  extends AccessPermissionInference[NumDom.P, MayPointToPolyhedraState, AccessPolyhedraState] {
  override def entryStateBuilder = AccessPolyhedraEntryStateBuilder
}

/** Access Permission Inference Runner.
  *
  * @tparam N the numerical domain
  * @tparam T the maypointto+numerical state
  * @tparam S the access permission state
  * @author Caterina Urban
  */
trait AccessPermissionInferenceRunner[N <: NumericalDomain[N], T <: MayPointToNumericalState[N,T], S <: AccessPermissionState[N,T,S]]
  extends SilAnalysisRunner[S] {

  /** The analysis to be run. */
  val analysis : AccessPermissionInference[N,T,S]

  /** Extends a sil.Program with permissions inferred by the PermissionAnalysis. */
  def extendProgram(prog: sil.Program, results: List[AnalysisResult[S]]): sil.Program = {
    // map of method names to control flow graphs
    val methodNameToCfgState = results.map(result => result.method.name.toString -> result.cfgState).toMap
    // extending program methods
    val extendMethods = prog.methods.map(
      method => methodNameToCfgState.get(method.name) match {
        case Some(cfgState) => extendMethod(method, cfgState)
        case None => method
      }
    )
    // building the extended program
    prog.copy(methods = extendMethods)(prog.pos, prog.info)
  }

  /** Extends a sil.Method with permissions inferred by the PermissionAnalysis. */
  def extendMethod(method: sil.Method, cfgState: AbstractCFGState[S]): sil.Method = {
    // retrieve the result of the analysis at the method entry
    val pre = cfgState.entryState()
    //println("PRE: " + pre)
    // update the method precondition
    var precondition: Seq[sil.Exp] = method.pres
    // add access preconditions
    for ((s,v) <- analysis.permissions.getOrElse(method.name.toString, Map[SymbolicAccess, Double]())) {
      // extracting all information from the path of the symbolic value
      val ids: List[Identifier] = s.path.reverse
      if (ids.length > 1) {
        // creating the corresponding field access
        val fst = sil.LocalVar(ids.head.getName)(typToSilver(ids.head.typ), ppToSilver(ids.head.pp))
        val snd = sil.FieldAccess(fst, sil.Field(ids.tail.head.getName, typToSilver(ids.tail.head.typ))())()
        val acc: sil.FieldAccess = ids.tail.tail.foldLeft[sil.FieldAccess](snd)(
          (exp, fld) => sil.FieldAccess(exp, sil.Field(fld.getName, typToSilver(fld.typ))())()
        )
        // adding access permission to the method precondition
        if (v == 1) {
          val perm = sil.FieldAccessPredicate(acc, sil.FullPerm()())()
          precondition = precondition ++ Seq[sil.Exp](perm)
        } else if (v > 0) {
          val (num, den) = AccessPermissionSolver.doubleToRational(v)
          val perm = sil.FieldAccessPredicate(acc, sil.FractionalPerm(sil.IntLit(num)(), sil.IntLit(den)())())()
          precondition = precondition ++ Seq[sil.Exp](perm)
        }
      }
    }

    // update the method body
    val body = extendStmt(method.body, method, cfgState)

    // retrieve the result of the analysis at the method exit
    val post = cfgState.exitState()
    //println("POST: " + post)
    // update the method postcondition
    var postcondition: Seq[sil.Exp] = method.posts
    // add access permissions
    //for ((id: FieldIdentifier,sym: Set[SymPermission]) <- post.idToSym) {
    //  // for each pair of identifier and set of symbolic permissions...
    //  val paths = post.heapNum.pathFromObj(id.obj) // retrieve the paths leading to the receiver of the field identifier
    //  //println("PATHS: " + paths)
    //  // select the shortest paths among the retrieved paths
    //  var shortest = Set[(VariableIdentifier,List[String])]()
    //  if (paths.size > 0) { // if there is at least one retrieved path
    //    // select the first path as the shortest
    //    shortest = Set[(VariableIdentifier,List[String])](paths.head)
    //    for ((x,p) <- paths.tail) { // for all remaining paths
    //      if (p.size < shortest.head._2.size) { // the current path is shorter than the ones collected so far
    //        // start collecting shorter paths
    //        shortest = Set[(VariableIdentifier,List[String])]((x,p))
    //      } else if (p.size == shortest.head._2.size) { // the current path is as long as the one collected so far
    //        shortest = shortest + ((x,p)) // add the current path to the collection
    //      }
    //    }
    //  }
    //  //println("SHORTEST: " + shortest)
    //  for ((x,p) <- shortest) { // for all retrieved shortest paths...
    //  // creating the corresponding field access
    //  val typ = typToSilver(x.typ)
    //    val fst = sil.LocalVar(x.toString)(typ, ppToSilver(x.pp))
    //    val ids = (id.field::p).reverse // note that the paths are stored in reverse order for efficiency
    //    val snd = sil.FieldAccess(fst, sil.Field(ids.head, typToSilver(id.typ))())()
    //    val acc: sil.FieldAccess = ids.tail.foldLeft[sil.FieldAccess](snd)(
    //      (exp, fld) => sil.FieldAccess(exp, sil.Field(fld, typ)())()
    //    )
    //    // adding access permission to the method postcondition
    //    val vls = analysis.permissions.getOrElse(method.name.toString, Map[SymbolicValue, Double]())
    //    // when access depends on multiple symbolic values (e.g., o.f -> { 1.0*acc(x.f), 1.0*acc(y.f) }) we take the minimum
    //    val v = sym.map(p => p.evaluate(vls)).min
    //    if (v >= 1) {
    //      val perm = sil.FieldAccessPredicate(acc, sil.FullPerm()())()
    //      postcondition = postcondition ++ Seq[sil.Exp](perm)
    //    } else if (v > 0) {
    //      val (num, den) = AccessPermissionSolver.doubleToRational(v)
    //      val perm = sil.FieldAccessPredicate(acc, sil.FractionalPerm(sil.IntLit(num)(), sil.IntLit(den)())())()
    //      postcondition = postcondition ++ Seq[sil.Exp](perm)
    //    }
    //  }
    //}
    //
    // return the method with updated precondition, updated body and updated postcondition
    method.copy(_pres = precondition, _body = body, _posts = postcondition)(method.pos, method.info)
  }

  def extendStmt(stmt: sil.Stmt, method: sil.Method, cfgState: AbstractCFGState[S]): sil.Stmt =
    stmt match {
  //    case stmt: sil.If => stmt
  //
  //    case stmt: sil.NewStmt =>
  //      val pos = DefaultSilConverter.convert(stmt.pos)
  //      println("POS: " + pos)
  //      val cfgPositions = cfgState.cfg.nodes.zipWithIndex.flatMap({
  //        case (stmts, blockIdx) => stmts.zipWithIndex.flatMap({
  //          case (stmt, stmtIdx) =>
  //            if (stmt.getPC() == pos) Some(CFGPosition(blockIdx, stmtIdx))
  //            else None
  //        })
  //      })
  //      //println("WHAT IS THIS: " + cfgPositions)
  //
  //      val lhs = stmt.lhs
  //      stmt
  //
  //    // TODO: wip
  //    case stmt: sil.Seqn =>
  //      val seq: Seq[sil.Stmt] = stmt.ss.foldRight(Seq[sil.Stmt]())(
  //        (s: sil.Stmt, ss: Seq[sil.Stmt]) => ss.+:(extendStmt(s,method,cfgState))
  //      )
  //      sil.Seqn(seq)(stmt.pos,stmt.info)
  //
  //    case stmt: sil.While =>
  //      // retrieve the position of the loop head
  //      val pos = DefaultSilConverter.convert(stmt.cond.pos)
  //      // retrieve the block index and the statement index within the block of the loop head
  //      val cfgPositions = cfgState.cfg.nodes.zipWithIndex.flatMap({
  //        case (stmts, blockIdx) => stmts.zipWithIndex.flatMap({
  //          case (stmt, stmtIdx) =>
  //            if (stmt.getPC() == pos) Some(CFGPosition(blockIdx, stmtIdx)) else None
  //        })
  //      })
  //      // retrieve the result of the analysis at the loop head
  //      val pre = cfgState.preStateAt(cfgPositions.head)
  //      // update the method loop invariants
  //      var invariants: Seq[sil.Exp] = stmt.invs
  //
  //      // add access permissions
  //      for ((id: FieldIdentifier,sym: Set[SymPermission]) <- pre.idToSym) {
  //        // for each pair of identifier and set of symbolic permissions...
  //        val paths = pre.heapNum.pathFromObj(id.obj) // retrieve the paths leading to the receiver of the field identifier
  //        // select the shortest paths among the retrieved paths
  //        var shortest = Set[(VariableIdentifier,List[String])]()
  //        if (paths.size > 0) { // if there is at least one retrieved path
  //          // select the first path as the shortest
  //          shortest = Set[(VariableIdentifier,List[String])](paths.head)
  //          for ((x,p) <- paths.tail) { // for all remaining paths
  //            if (p.size < shortest.head._2.size) { // the current path is shorter than the ones collected so far
  //              // start collecting shorter paths
  //              shortest = Set[(VariableIdentifier,List[String])]((x,p))
  //            } else if (p.size == shortest.head._2.size) { // the current path is as long as the one collected so far
  //              shortest = shortest + ((x,p)) // add the current path to the collection
  //            }
  //          }
  //        }
  //        for ((x,p) <- shortest) { // for all retrieved shortest paths...
  //        // creating the corresponding field access
  //        val typ = typToSilver(x.typ)
  //          val fst = sil.LocalVar(x.toString)(typ, ppToSilver(x.pp))
  //          val ids = (id.field::p).reverse // note that the paths are stored in reverse order for efficiency
  //          val snd = sil.FieldAccess(fst, sil.Field(ids.head, typToSilver(id.typ))())()
  //          val acc: sil.FieldAccess = ids.tail.foldLeft[sil.FieldAccess](snd)(
  //            (exp, fld) => sil.FieldAccess(exp, sil.Field(fld, typ)())()
  //          )
  //          // adding access permission to the loop invariants
  //          val vls = analysis.permissions.getOrElse(method.name.toString, Map[SymbolicValue, Double]())
  //          // when access depends on multiple symbolic values (e.g., o.f -> { 1.0*acc(x.f), 1.0*acc(y.f) }) we take the minimum
  //          val v = sym.map(p => p.evaluate(vls)).min
  //          if (v >= 1) {
  //            val perm = sil.FieldAccessPredicate(acc, sil.FullPerm()())()
  //            invariants = invariants ++ Seq[sil.Exp](perm)
  //          } else if (v > 0) {
  //            val (num, den) = AccessPermissionSolver.doubleToRational(v)
  //            val perm = sil.FieldAccessPredicate(acc, sil.FractionalPerm(sil.IntLit(num)(), sil.IntLit(den)())())()
  //            invariants = invariants ++ Seq[sil.Exp](perm)
  //          }
  //        }
  //      }
  //
  //      // add equalities between paths within the heap
  //      var eqSet = Set[sil.Exp]() // current set of equalities to add
  //      for (obj: HeapIdentifier <- pre.heapNum.objFieldToObj.keys) { // for all heap objects...
  //      val paths = pre.heapNum.pathFromObj(obj) // retrieve paths leading to the given heap object
  //        if (paths.size > 1) { // if there are at least two different paths...
  //        var curr = paths.head // first path
  //        var currIds = curr._2.drop(1).foldLeft(pre.heapNum.refToObj(curr._1))(
  //            (set,next) => // next path segment
  //              set.foldLeft(Set[HeapIdentifier]())(
  //                (s,obj) => s ++ pre.heapNum.objFieldToObj.getOrElse(obj,Map[String,Set[HeapIdentifier]]()).
  //                  getOrElse(next,Set[HeapIdentifier]())
  //              )
  //          ) // evaluate the first path
  //          for (p <- paths - curr) { // second path
  //          // evaluate the second path
  //          val pIds = p._2.drop(1).foldLeft(pre.heapNum.refToObj(p._1))(
  //              (set,next) => // next path segment
  //                set.foldLeft(Set[HeapIdentifier]())(
  //                  (s,obj) => s ++ pre.heapNum.objFieldToObj.getOrElse(obj,Map[String,Set[HeapIdentifier]]()).
  //                    getOrElse(next,Set[HeapIdentifier]())
  //                )
  //            )
  //            if (currIds == pIds) {
  //              // creating the field access corresponding to the first path
  //              val currTyp = typToSilver(curr._1.typ)
  //              val currFst = sil.LocalVar(curr._1.toString)(currTyp, ppToSilver(curr._1.pp))
  //              var currExp: sil.Exp = currFst
  //              if (curr._2.size > 1) {
  //                val currNms = curr._2.tail.reverse // note that the paths are stored in reverse order for efficiency
  //                val currSnd = sil.FieldAccess(currFst, sil.Field(currNms.head, currTyp)())()
  //                val currAcc: sil.FieldAccess = currNms.tail.foldLeft[sil.FieldAccess](currSnd)(
  //                  (exp, fld) => sil.FieldAccess(exp, sil.Field(fld, currTyp)())()
  //                )
  //                currExp = currAcc
  //              }
  //              // creating the field access corresponding to the second path
  //              val pTyp = typToSilver(p._1.typ)
  //              val pFst = sil.LocalVar(p._1.toString)(pTyp, ppToSilver(p._1.pp))
  //              var pExp: sil.Exp = pFst
  //              if (p._2.size > 1) {
  //                val pNms = p._2.tail.reverse // note that the paths are stored in reverse order for efficiency
  //                val pSnd = sil.FieldAccess(pFst, sil.Field(pNms.head, pTyp)())()
  //                val pAcc: sil.FieldAccess = pNms.tail.foldLeft[sil.FieldAccess](pSnd)(
  //                  (exp, fld) => sil.FieldAccess(exp, sil.Field(fld, pTyp)())())
  //                pExp = pAcc
  //              }
  //              // adding equality to invariants
  //              eqSet = eqSet + sil.EqCmp(currExp,pExp)()
  //            }
  //            curr = p; currIds = pIds
  //          }
  //        }
  //      }
  //      for (eq <- eqSet) { invariants = invariants ++ Seq[sil.Exp](eq) }
  //
  //      sil.While(stmt.cond, invs = invariants, stmt.locals, body = extendStmt(stmt.body,method,cfgState))(stmt.pos,stmt.info)
  //
      case _ => stmt //println(stmt.getClass); stmt
    }

  def ppToSilver(pp: ProgramPoint): sil.Position = pp match { // convert sample.ProgramPoint to sil.ProgramPoint
    case sample.DummyProgramPoint => sil.NoPosition
    case sample.WrappedProgramPoint(pos) => pos.asInstanceOf[SourcePosition]
  }
  def typToSilver(typ: Type): sil.Type = typ match { // convert sample.Type to sil.Type
    case sample.IntType => sil.Int
    case sample.BoolType => sil.Bool
    case sample.RefType(_) => sil.Ref
  }

  override def main(args: Array[String]) {
    // run the analysis and the permission inference
    val results = run(new File(args(0)).toPath)
    println("\n******************\n* AnalysisResult *\n******************\n")
    for (w <- Reporter.seenInfos) {
      println(w)
    }
    if (Reporter.seenErrors.isEmpty) println("No errors")
    for (e <- Reporter.seenErrors) {
      println(e)
    }
    // extend program with inferred permission
    val out = extendProgram(DefaultSilConverter.prog,results)
    println("\n********************\n* Extended Program *\n********************\n\n" + out)
    // create a file with the extended program
    val outName = args(0).split('.')(0) + "X.sil"
    val pw = new PrintWriter(new File(outName))
    pw.write(out.toString)
    pw.close
    // verify the extended program with silicon
    val silicon = new Silicon(Seq(("startedBy", "viper.silicon.SiliconTests")))
    silicon.parseCommandLine(Seq("dummy.sil"))
    silicon.config.initialize { case _ => silicon.config.initialized = true }
    silicon.start()
    val result: viper.silver.verifier.VerificationResult = silicon.verify(out)
    println("\n***********************\n* Verification Result * " + result + "\n***********************")
  }

  override def toString = "Access Permission Inference"
}

/** Access Permission Inference Runner using Intervals.
  *
  * @author Caterina Urban
  */
object AccessIntervalsAnalysisRunner
  extends AccessPermissionInferenceRunner[NumDom.I, MayPointToIntervalsState, AccessIntervalsState] {
  override val analysis = AccessIntervalsInference
  override def toString = "Access Permission Inference using Intervals"
}

/** Access Permission Inference Runner using Intervals.
  *
  * @author Caterina Urban
  */
object AccessPolyhedraAnalysisRunner
  extends AccessPermissionInferenceRunner[NumDom.P, MayPointToPolyhedraState, AccessPolyhedraState] {
  override val analysis = AccessPolyhedraInference
  override def toString = "Access Permission Inference using Polyhedra"
}
