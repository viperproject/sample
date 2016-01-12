package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.execution.{SimpleAnalysis, AnalysisResult, EntryStateBuilder}
import ch.ethz.inf.pm.sample.oorepresentation.sil.SilAnalysisRunner
import ch.ethz.inf.pm.sample.oorepresentation.{MethodDeclaration, ProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging

/** A path is a sequence starting with a variable and followed by field identifiers (e.g., x.f)
  *
  * @param p the list of strings forming the path
  * @author Caterina Urban
  */
class Path(val p : List[String]) {
  override def toString : String = p.mkString(".")
  override def equals(a: Any) : Boolean = a match {
    case x: Path => p.corresponds(x.p)(_ equals _)
    case _ => false
  }
}

/** We introduce a symbolic value for each location
  * and each possible occurrence of an access permission in a pre- or post-condition, or monitor invariant.
  *
  * @param path the path for which we specify the access permission
  * @author Caterina Urban
  */
sealed abstract class SymbolicValue(var path : Path) {
  def setPath(p : Path) : SymbolicValue =
    if (path == null) {
      path=p; this
    } else throw new RuntimeException("The path of the symbolic value is already initialized.")
  def factory() : SymbolicValue
}

case class SymbolicPermissionPredicate(p: Path) extends SymbolicValue(p) {
  override def toString : String = "Permission(" + path.toString() + ")"
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicPermissionPredicate => path.equals(x.path)
    case _ => false
  }
  override def factory() : SymbolicValue = new SymbolicPermissionPredicate(p)
}

/** Symbolic permission monomial
  *
  * @param n the number of times the symbolic value is taken into account
  * @param s symbolic value taken into account
  * @author Caterina Urban
  */
class CountedSymbolicValues(val n : Double, val s : SymbolicValue) {

  def -(b : CountedSymbolicValues) = {
    assert(this.sameSymbolicValue(b))
    new CountedSymbolicValues(this.n-b.n, this.s)
  }
  def +(b : CountedSymbolicValues) = {
    assert(this.sameSymbolicValue(b))
    new CountedSymbolicValues(this.n+b.n, this.s)
  }

  override def equals(a : Any) : Boolean = a match {
    case b: CountedSymbolicValues =>
      n.equals(b.n) && ((b.s==null && s==null) || (b.s!=null && s!=null && s.equals(b.s)))
    case _ => false
  }

  def glb(a : CountedSymbolicValues, b : CountedSymbolicValues) = {
    assert(a.sameSymbolicValue(b))
    new CountedSymbolicValues(Math.max(a.n, b.n), a.s)
  }
  def lub(a : CountedSymbolicValues, b : CountedSymbolicValues) = {
    assert(a.sameSymbolicValue(b))
    new CountedSymbolicValues(Math.min(a.n, b.n), a.s)
  }

  def sameSymbolicValue(a : CountedSymbolicValues) : Boolean = {
    if (this.s == null && a.s == null) return true
    if (this.s == null || a.s == null) return false
    this.s.equals(a.s)
  }

  override def toString = s match {
    case null => n.toString
    case k => n.toString + "*" + s.toString
  }
}

/** Symbolic permission.
  *
  * @author Caterina Urban
  */
class SymbolicPermission extends Lattice[SymbolicPermission] {

  var value : Set[CountedSymbolicValues] = Set.empty[CountedSymbolicValues]

  /** Returns the bottom value of the lattice. */
  override def bottom(): SymbolicPermission = ???
  /** Returns a new instance of the lattice. */
  override def factory(): SymbolicPermission = ???
  /** Returns the top value of the lattice. */
  override def top(): SymbolicPermission = ???

  /** Checks whether the given domain element is equivalent to bottom. */
  override def isBottom: Boolean = false
  /** Returns true if and only if `this` is less than or equal to `other`. */
  override def lessEqual(other: SymbolicPermission): Boolean = ???
  /** Checks whether the given domain element is equivalent to top. */
  override def isTop: Boolean = ???

  /** Computes the greatest lower bound of two elements. */
  override def glb(other: SymbolicPermission): SymbolicPermission = ???
  /** Computes the least upper bound of two elements. */
  override def lub(other: SymbolicPermission): SymbolicPermission = ???
  /** Computes the widening of two elements. */
  override def widening(other: SymbolicPermission): SymbolicPermission = ???

  override def toString : String = if (isBottom) "⊥" else value.mkString("", " + ", "0")
}

/** Permission Inference State.
  *
  * Note that each abstract state must include an `ExpressionSet`!
  * It is accessed during the analysis to retrieve the result of each statement!!
  *
  * @author Caterina Urban
  */
case class PermissionState(heapNum: PointsToNumericalState)
  extends SimplePermissionState[PermissionState]
  with StateWithBackwardAnalysisStubs[PermissionState]
  with LazyLogging
{
  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  override def assignField(obj: Expression, field: String, right: Expression): PermissionState = {
    logger.debug("*** assignField(" + obj.toString + "; " + field.toString + "; " + right.toString + ")")

    obj match {
      case obj: FieldIdentifier =>

        val sym = new SymbolicPermissionPredicate(new Path(List(obj.obj.getName,obj.field)))
        val c = PermissionSolver.permissionType.ensureWrite(Mul(1,sym))
        PermissionSolver.addConstraint(c)

        this.copy(heapNum = heapNum.assignField(obj, field, right))
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
  override def assignVariable(x: Expression, right: Expression): PermissionState = {
    val idx = x match {
      case _: VariableIdentifier => "VariableIdentifier"
      case _ => "Unknown"
    }
    val idright = right match {
      case _: FieldIdentifier => "FieldIdentifier"
      case _: HeapIdentifier => "HeapIdentifier"
      case _: VariableIdentifier => "VariableIdentifier"
      case _ => "Unknown: " + right.getClass.getSimpleName
    }
    logger.debug("*** assignVariable(" + x.toString + ": " + idx + "; " + right.toString + ": " + idright + ")")

//    x match {
//      case x: VariableIdentifier =>
//        if (x.typ.isObject) { // the assigned variable is a `Ref`
//          right match {
//            case right: FieldIdentifier => // e.g., `x := y.g`
//              val s = this.objFieldToObj(right.obj)(right.field) // retrieve the heap `Obj` objects
//              // add xref -> s to refToObj map
//              val refToObjmap = this.refToObj + (x -> s)
//              // return the current state with updated refToObj
//              this.copy(refToObj = refToObjmap)
//
//            case right: HeapIdentifier => // e.g., `x = new()`
//              // add xref -> right to refToObj map
//              val refToObjmap = this.refToObj + (x -> Set[HeapIdentifier](right))
//              // add key to objFieldToObj map
//              val objFieldToObjmap = this.objFieldToObj + (right -> Map[String,Set[HeapIdentifier]]())
//              // return the current state with updated refToObj and updated objFieldToObj
//              this.copy(refToObj = refToObjmap, objFieldToObj = objFieldToObjmap)
//
//            case right: VariableIdentifier => // e.g., `x := y`
//              // add xref -> refToObj[rightref] to refToObj map
//              val refToObjmap = this.refToObj + (x -> this.refToObj.getOrElse(right, Set[HeapIdentifier]()))
//              // return the current state with updated refToObj
//              this.copy(refToObj = refToObjmap)
//
//            case _ => throw new NotImplementedError("A variable assignment implementation is missing.")
//          }
//        } else {  // the assigned variable is not a `Ref`
//          // return the current state with updated numDom
//          this.copy(numDom = numDom.assign(x,right))
//        }
//      case _ => throw new IllegalArgumentException("A variable assignment must occur via a VariableIdentifier.")
//    }

    this.copy(heapNum = heapNum.assignVariable(x, right))
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): PermissionState = {
    logger.debug("*** assume(" + cond.toString + ")")

//    cond match {
//      // Constant
//      case cond:Constant => // return the current state with updated numDom
//        this.copy(numDom = numDom.assume(cond))
//
//      // Identifier
//      case cond: Identifier => // return the current state with updated numDom
//        this.copy(numDom = numDom.assume(cond))
//
//      // BinaryArithmeticExpression
//      case cond: BinaryArithmeticExpression => // return the current state with updated numDom
//        this.copy(numDom = numDom.assume(cond))
//
//      // BinaryBooleanExpression
//      case BinaryBooleanExpression(left, right, BooleanOperator.&&, typ) =>
//        if (cond.canonical) // return the current state with updated numDom
//          this.copy(numDom = numDom.assume(cond))
//        else
//          this.assume(left).assume(right)
//      case BinaryBooleanExpression(left, right, BooleanOperator.||, typ) =>
//        if (cond.canonical) // return the current state with updated numDom
//          this.copy(numDom = numDom.assume(cond))
//        else
//          this.assume(left) lub this.assume(right)
//
//      // NegatedBooleanExpression
//      case cond:NegatedBooleanExpression => {
//        cond.exp match {
//          // Constant
//          case c: Constant => // return the current state with updated numDom
//            this.copy(numDom = numDom.assume(cond))
//
//          // Identifier (i.e., FieldIdentifier, VariableIdentifier)
//          case id: Identifier => // return the current state with updated numDom
//            this.copy(numDom = numDom.assume(cond))
//
//          // BinaryArithmeticExpression
//          case BinaryArithmeticExpression(left, right, op, typ) =>
//            this.assume(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op), typ))
//
//          // BinaryBooleanExpression
//          case BinaryBooleanExpression(left, right, op, typ) =>
//            val nleft = NegatedBooleanExpression(left)
//            val nright = NegatedBooleanExpression(right)
//            val nop = op match {
//              case BooleanOperator.&& => BooleanOperator.||
//              case BooleanOperator.|| => BooleanOperator.&&
//            }
//            this.assume(BinaryBooleanExpression(nleft, nright, nop, typ))
//
//          // NegatedBooleanExpression
//          case NegatedBooleanExpression(exp) => this.assume(exp)
//
//          // ReferenceComparisonExpression
//          case ReferenceComparisonExpression(left, right, op, typ) =>
//            val nop = op match {
//              case ArithmeticOperator.== => ArithmeticOperator.!=
//              case ArithmeticOperator.!= => ArithmeticOperator.==
//            }
//            this.assume(ReferenceComparisonExpression(left, right, nop, typ))
//
//          case _ => throw new NotImplementedError("An assumeNegatedBooleanExpression implementation for "
//            + cond.exp.getClass.getSimpleName + " is missing.")
//        }
//      }
//
//      case ReferenceComparisonExpression(left, right, op, typ) =>
//        if (op == ArithmeticOperator.==) {
//          println(left.getClass.getSimpleName, left.typ)
//          println(right.getClass.getSimpleName, right.typ)
//          this
//        } else { // cond.op == ArithmeticOperator.!=
//          println(left.getClass.getSimpleName, left.typ)
//          println(right.getClass.getSimpleName, right.typ)
//          this
//        }
//
//      case _ => throw new NotImplementedError("An assume implementation is missing.")
//    }

    this.copy(heapNum = heapNum.assume(cond))
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): PermissionState = {
    logger.debug("*** before(" + pp.toString + ")")

    this  // return the current state without modification
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): PermissionState = {
    logger.debug("*** bottom()")

//    // return a new state with bottom exprSet and empty refToObj map
//    PermissionState(exprSet.bottom(),refToObj.empty,objFieldToObj.empty,numDom.bottom())
    this.copy(heapNum = heapNum.bottom())
  }

  /** Creates an object at allocation site.
    *
    * Invoked by calls to `new()`.
    *
    * @param typ The dynamic type of the created object
    * @param pp The allocation site of the object
    * @return The abstract state with updated exprSet after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** createObject(" + typ.toString + "; " + pp.toString + ")")
    
//    val obj = HeapIdentifier(typ,pp) // create new Obj
//    val objFieldToObjmap = objFieldToObj + (obj -> Map[String,Set[HeapIdentifier]]()) // add key to objFieldToObj map
//    // return the current state with updated exprSet and updated objFieldToObj map
//    this.copy(exprSet = ExpressionSet(obj), objFieldToObj = objFieldToObjmap)

    this.copy(heapNum = heapNum.createObject(typ, pp))
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
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** createVariable(" + x.toString + "; " + typ.toString + "; " + pp.toString + ")")

//    if (typ.isObject) { // the variable to be created is a `Ref`
//      val refToObjmap = refToObj + (x -> Set[HeapIdentifier]()) // add key to refToObj map
//      // return the current state with updated exprSet and refToObj
//      this.copy(exprSet = ExpressionSet(x), refToObj = refToObjmap)
//    } else { // the variable to be created is not a `Ref`
//      // return the current state with updated numDom
//      this.copy(numDom = numDom.createVariable(x,typ))
//    }

    this.copy(heapNum = heapNum.createVariable(x, typ, pp))
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): PermissionState = {
    logger.debug("*** createVariableForArgument(" + x.toString + "; " + typ.toString + ")")

//    if (typ.isObject) { // the variable to be created is a `Ref`
//      val obj = HeapIdentifier(typ,x.pp) // create new Obj
//      val refToObjmap = refToObj + (x -> Set[HeapIdentifier](obj)) // add key to refToObj map
//      val objFieldToObjmap = objFieldToObj + (obj -> Map[String,Set[HeapIdentifier]]()) // add key to objFieldToObj map
//      // return the current state with updated exprSet, updated refToObj and updated objFieldToObj
//      this.copy(exprSet = ExpressionSet(x), refToObj = refToObjmap, objFieldToObj = objFieldToObjmap)
//    } else { // the variable to be created is not a `Ref`
//      // return the current state with updated numDom
//      this.copy(numDom = numDom.createVariable(x,typ))
//    }

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
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** evalConstant(" + value + "; " + typ.toString + "; " + pp.toString + ")")

//    val const = new Constant(value, typ, pp)
//    // return the current state with updated exprSet
//    this.copy(exprSet = ExpressionSet(const))

    this.copy(heapNum = heapNum.evalConstant(value, typ, pp))
  }

  /** Exhales permissions. */
  override def exhale(acc: Expression) : PermissionState = {
    logger.debug("*** exhale(" + acc.toString + "): implement me!")

    this
  }

  /** The current expression.
    *
    * Invoked after each statement to retrieve its result.
    */
  override def expr: ExpressionSet = {
    logger.debug("*** expr: " + this.heapNum.exprSet.toString)
    
    this.heapNum.exprSet // return exprSet
  }

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    *
    * @todo implement me!
    */
  override def factory(): PermissionState = {
    logger.debug("*** factory(): implement me!")
    
    this.copy(heapNum = heapNum.factory())
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
  override def getFieldValue(obj: Expression, field: String, typ: Type): PermissionState = {
    logger.debug("*** getFieldValue(" + obj.toString + "; " + field + "; " + typ.toString + ")")

    obj match {
      case obj:AccessPathIdentifier =>

        val sym = new SymbolicPermissionPredicate(new Path(obj.stringPath))
        val c = PermissionSolver.permissionType.ensureRead(Mul(1,sym))
        PermissionSolver.addConstraint(c)

        // return the current state with updated updated heapNum
        this.copy(heapNum = heapNum.getFieldValue(obj, field, typ))

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
  override def getVariableValue(id: Identifier): PermissionState = {
    logger.debug("*** getVariableValue(" + id.toString + ")")

//    // return the current state with updated exprSet
//    this.copy(exprSet = ExpressionSet(id))

    this.copy(heapNum = heapNum.getVariableValue(id))
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    *
    * @todo implement me!
    */
  override def glb(other: PermissionState): PermissionState = {
    logger.debug("*** glb(" + other.repr + "): implement me!")
    
    this.copy(heapNum = heapNum glb other.heapNum)
  }

  /** Inhales permissions. */
  override def inhale(acc: Expression) : PermissionState = {
    logger.debug("*** inahle(" + acc.toString + "): implement me!")

    this
  }

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return `true` if and only if the state is equivalent to bottom
    *
    * @todo implement me!
    */
  override def isBottom: Boolean = {
    logger.debug("*** isBottom: implement me!")

    heapNum.isBottom
  }

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return `true` if and only if the state is equivalent to top
    *
    * @todo implement me!
    */
  override def isTop: Boolean = {
    logger.debug("*** isTop: implement me!")
    
    heapNum.isTop
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return `true` if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: PermissionState): Boolean = {
    logger.debug("*** lessEqual(" + other.repr + ")")

//    val exp = this.exprSet.lessEqual(other.exprSet) // test the exprSets
//    val refToObjmap = this.refToObj.forall {
//      case (k: VariableIdentifier,v: Set[HeapIdentifier]) => v subsetOf other.refToObj.getOrElse(k,Set[HeapIdentifier]())
//    } // test the refToObjs
//    val objFieldToObjmap = this.objFieldToObj.forall {
//      case (o: HeapIdentifier, m: Map[String,Set[HeapIdentifier]]) => m.forall {
//        case (f: String, s: Set[HeapIdentifier]) =>
//          s subsetOf other.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()).getOrElse(f,Set[HeapIdentifier]())
//      }
//    } // test the objFieldToObjs
//    val num = this.numDom.lessEqual(other.numDom) // test the numDoms
//    exp && refToObjmap && objFieldToObjmap && num

    heapNum lessEqual other.heapNum
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: PermissionState): PermissionState = {
    logger.debug("*** lub(" + other.repr + ")")

//    val exp = this.exprSet lub other.expr // join the exprSets
//    val refToObjmap = this.refToObj ++ other.refToObj.map {
//      case (k: VariableIdentifier,v: Set[HeapIdentifier]) => k -> (this.refToObj.getOrElse(k,Set[HeapIdentifier]()) ++ v)
//    } // merge the refToObjs
//    val objFieldToObjmap = this.objFieldToObj ++ other.objFieldToObj.map {
//      case (o: HeapIdentifier,m: Map[String,Set[HeapIdentifier]]) => o ->
//        (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()) ++ other.objFieldToObj(o).map {
//          case (s: String, v: Set[HeapIdentifier]) => s ->
//            (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()).getOrElse(s,Set[HeapIdentifier]()) ++ v)
//        })
//    } // merge the objFieldToObjmap
//    val num = this.numDom lub other.numDom // join the numDoms
//
//    // return the current state with updated exprSet, updated refToObj, updated objFieldToObjmap and updated numDom
//    this.copy(exprSet = exp, refToObj = refToObjmap, objFieldToObj = objFieldToObjmap, numDom = num)

    this.copy(heapNum = heapNum lub other.heapNum)
  }

  /** Performs abstract garbage collection.
    *
    * @todo implement me!
    */
  override def pruneUnreachableHeap(): PermissionState = {
    logger.debug("*** pruneUnreachableHeap(): implement me!")

    this.copy(heapNum = heapNum.pruneUnreachableHeap())
  }

  /** Removes all variables satisfying filter.
    *
    * @todo implement me!
    */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): PermissionState = {
    logger.debug("*** pruneVariables(" + filter.toString + "): implement me!")
    
    this.copy(heapNum = heapNum.pruneVariables(filter))
  }

  /** Removes the current expression.
    *
    * @return The abstract state obtained after removing the current expression
    */
  override def removeExpression(): PermissionState = {
    logger.debug("*** removeExpression()")

//    // return the current state with a new exprSet
//    this.copy(exprSet = ExpressionSet())

    this.copy(heapNum = heapNum.removeExpression())
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    *
    * @todo implement me!
    */
  override def removeVariable(varExpr: VariableIdentifier): PermissionState = {
    logger.debug("*** removeVariable(" + varExpr.toString + "): implement me!")
    
    this.copy(heapNum = heapNum.removeVariable(varExpr))
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
      heapNum.objFieldToObj.toString + ", " +
      heapNum.numDom.toString + ")" +
    ")"
  }

  /** Assigns an expression to an argument.
    *
    * @param x The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    *
    * @todo implement me!
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): PermissionState = {
    logger.debug("*** setArgument(" + x.toString + "; " + right.toString + "): implement me!")
    
    this.copy(heapNum = heapNum.setArgument(x, right))
  }

  /** Sets the current expression.
    *
    * Invoked after statements that do not have results (like assignments to variables and fields).
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): PermissionState = {
    logger.debug("*** setExpression(" + expr.toString + ")")
    
//    this.copy(exprSet = expr) // return the current state with updated exprSet

    this.copy(heapNum = heapNum.setExpression(expr))
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    *
    * @todo implement me!
    */
  override def setVariableToTop(varExpr: Expression): PermissionState = {
    logger.debug("*** setVariableToTop(" + varExpr.toString + "): implement me!")
    
    this.copy(heapNum = heapNum.setVariableToTop(varExpr))
  }

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    *
    * @todo implement me!
    */
  override def throws(t: ExpressionSet): PermissionState = {
    logger.debug("*** throws(" + t.toString + "): implement me!")
    
    this.copy(heapNum = heapNum.throws(t))
  }

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    *
    * @todo implement me!
    */
  override def top(): PermissionState = {
    logger.debug("*** top(): implement me!")
    
    this.copy(heapNum = heapNum.top())
  }

  /** The state string representation.
    *
    * @return the string representation of the current state
    */
  override def toString: String = "PermissionState(" + heapNum.toString + ")"

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    *
    * @todo implement me!
    */
  override def widening(other: PermissionState): PermissionState = {
    logger.debug("*** widening(" + other.repr + ")")

    //    val exp = this.exprSet widening other.expr // widen the exprSets
    //    val refToObjmap = this.refToObj ++ other.refToObj.map {
    //        case (k: VariableIdentifier,v: Set[HeapIdentifier]) => k -> (this.refToObj.getOrElse(k,Set[HeapIdentifier]()) ++ v)
    //      } // merge the refToObjs
    //    val objFieldToObjmap = this.objFieldToObj ++ other.objFieldToObj.map {
    //        case (o: HeapIdentifier,m: Map[String,Set[HeapIdentifier]]) => o ->
    //          (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()) ++ other.objFieldToObj(o).map {
    //            case (s: String, v: Set[HeapIdentifier]) => s ->
    //              (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()).getOrElse(s,Set[HeapIdentifier]()) ++ v)
    //          })
    //      } // merge the objFieldToObjmap
    //    val num = this.numDom widening other.numDom // widen the numDoms
    //
    //    // return the current state with updated exprSet, updated refToObj, updated objFieldToObjmap and updated numDom
    //    this.copy(exprSet = exp, refToObj = refToObjmap, objFieldToObj = objFieldToObjmap, numDom = num)
    //  }

    this.copy(heapNum = heapNum widening other.heapNum)
  }
}

/** Builds permission analysis entry states for given method declarations. */
object PermissionEntryStateBuilder extends EntryStateBuilder[PermissionState] {
  override def topState: PermissionState = PermissionState(PointsToNumericalState(ExpressionSet(),
    Map[VariableIdentifier,Set[HeapIdentifier]](),
    Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]](),
    Apron.Polyhedra.Bottom.factory()))
}

class PermissionAnalysis extends SimpleAnalysis[PermissionState](PermissionEntryStateBuilder) {
  override def analyze(method: MethodDeclaration): AnalysisResult[PermissionState] = {
    val result = analyze(method, entryStateBuilder.build(method))

    val solution = PermissionSolver.solve(PermissionSolver.getConstraints)
    println(solution.mapValues((d) => PermissionSolver.doubleToRational(d)))

    //val cfg = result.cfgState

    //if (cfg.cfg.initialBlockInLoop(0)) println("0 is loop")
    //if (cfg.cfg.initialBlockInLoop(1)) println("1 is loop")
    //if (cfg.cfg.initialBlockInLoop(2)) println("2 is loop")
    //if (cfg.cfg.initialBlockInLoop(3)) println("3 is loop")
    //if (cfg.cfg.initialBlockInLoop(4)) println("4 is loop")
    //println(cfg.blockStates)
    //println("states: " + cfg.blockStates.mapValues[PermissionState](
    //  (l) => l.last.head
    //))

    //println("Analysis Result:\n" + result)
    result
  }
}

/** Runs the Access Permission Inference analysis. */
object PermissionAnalysisRunner extends SilAnalysisRunner[PermissionState] {
  val analysis = new PermissionAnalysis

  override def toString = "Access Permission Inference Analysis"
}

