package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.execution.{SimpleAnalysis, AnalysisResult, EntryStateBuilder}
import ch.ethz.inf.pm.sample.oorepresentation.sil.SilAnalysisRunner
import ch.ethz.inf.pm.sample.oorepresentation.{MethodDeclaration, LineColumnProgramPoint, ProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging

/** Object created at object allocation site.
  *
  * @param typ the type of the object
  * @param pp the object allocation site
  * @author Caterina Urban
  */
case class HeapIdentifier(typ: Type, pp: ProgramPoint) extends Identifier {
  /** The name of the heap identifier. */
  override def getName: String = "O" + number
  private def number : String = pp match {
    case pp:LineColumnProgramPoint => pp.getLine.toString
    case _ => pp.description
  }
  /** The name of the field represented by the identifier. */
  override def getField: Option[String] = None
  /** Whether the identifier is a summary node. */
  override def representsSingleVariable: Boolean = false
}

/** Field of an object.
  *
  * @param obj the object
  * @param field the field name
  * @author Caterina Urban
  */
case class FieldIdentifier(obj: HeapIdentifier, field: String, typ: Type) extends Identifier {
  /** The name of the object field. */
  override def getName: String = obj.getName + "." + field
  /** The name of the field represented by the identifier. */
  override def getField: Option[String] = Some(field)
  /** Whether the identifier is a summary node. */
  override def representsSingleVariable: Boolean = obj.representsSingleVariable
  /** Point in the program where the identifier is located. */
  override def pp: ProgramPoint = obj.pp
}

/** Permission Inference State
  *
  * Note that each abstract state must include an `ExpressionSet`!
  * It is accessed during the analysis to retrieve the result of each statement!!
  *
  * @author Caterina Urban
  */
case class PermissionState(exprSet: ExpressionSet,
                           // map from `Ref` variables to heap `Obj` objects
                           refToObj: Map[VariableIdentifier,Set[HeapIdentifier]],
                           // map from heap `Obj` objects to a map from `Ref` fields to heap `Obj` objects
                           objFieldToObj: Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]],
                           // polyhedra abstract domain
                           numDom : Apron.Polyhedra)
  extends SimpleState[PermissionState]
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
        if (obj.typ.isObject) { // the assigned field is a `Ref`
          right match {

            case right: FieldIdentifier => // e.g., `x.f := y.g`
              val s = objFieldToObj(right.obj)(right.field) // retrieve the heap `Obj` objects
              val o = obj.obj // retrieve `Obj` whose field is assigned
              val f = obj.field // retrieve assigned field
              // weak update
              val objFieldToObjmap = objFieldToObj + (o -> (objFieldToObj(o) + (f -> (objFieldToObj(o)(f) ++ s))))
              // return the current state with updated objFieldToObj
              this.copy(objFieldToObj = objFieldToObjmap)

            case right: HeapIdentifier => // e.g., `x.f := new()`
              val o = obj.obj // retrieve `Obj` whose field is assigned
              val f = obj.field // retrieve assigned field
              val objFieldToObjmap = objFieldToObj +
                // add key to objFieldToObjmap
                (right -> Map[String,Set[HeapIdentifier]]()) +
                // weak update
                (o -> (objFieldToObj(o) + (f -> (objFieldToObj(o)(f) + right))))
              // return the current state with updated objFieldToObj
              this.copy(objFieldToObj = objFieldToObjmap)

            case right: VariableIdentifier => // e.g., `x.f := y`
              val s = refToObj(right) // retrieve the corresponding heap `Obj` objects
              val o = obj.obj // retrieve `Obj` whose field is assigned
              val f = obj.field // retrieve assigned field
              // weak update
              val objFieldToObjmap = objFieldToObj + (o -> (objFieldToObj(o) + (f -> (objFieldToObj(o)(f) ++ s))))
              // return the current state with updated objFieldToObj
              this.copy(objFieldToObj = objFieldToObjmap)

            case _ => throw new NotImplementedError("A field assignment implementation is missing.")
          }
        } else {  // the assigned field is not a `Ref`
          // weak update
          val num = numDom lub numDom.assign(obj,right)
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

    x match {
      case x: VariableIdentifier =>
        if (x.typ.isObject) { // the assigned variable is a `Ref`
          right match {
            case right: FieldIdentifier => // e.g., `x := y.g`
              val s = this.objFieldToObj(right.obj)(right.field) // retrieve the heap `Obj` objects
              // add xref -> s to refToObj map
              val refToObjmap = this.refToObj + (x -> s)
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap)

            case right: HeapIdentifier => // e.g., `x = new()`
              // add xref -> right to refToObj map
              val refToObjmap = this.refToObj + (x -> Set[HeapIdentifier](right))
              // add key to objFieldToObj map
              val objFieldToObjmap = this.objFieldToObj + (right -> Map[String,Set[HeapIdentifier]]())
              // return the current state with updated refToObj and updated objFieldToObj
              this.copy(refToObj = refToObjmap, objFieldToObj = objFieldToObjmap)

            case right: VariableIdentifier => // e.g., `x := y`
              // add xref -> refToObj[rightref] to refToObj map
              val refToObjmap = this.refToObj + (x -> this.refToObj.getOrElse(right, Set[HeapIdentifier]()))
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap)

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
  override def assume(cond: Expression): PermissionState = {
    logger.debug("*** assume(" + cond.toString + ")")

    cond match {
      // Constant
      case cond:Constant => // return the current state with updated numDom
        this.copy(numDom = numDom.assume(cond))

      // Identifier
      case cond: Identifier => // return the current state with updated numDom
        this.copy(numDom = numDom.assume(cond))

      // BinaryArithmeticExpression
      case cond: BinaryArithmeticExpression => // return the current state with updated numDom
        this.copy(numDom = numDom.assume(cond))

      // BinaryBooleanExpression
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, typ) =>
        if (cond.canonical) // return the current state with updated numDom
          this.copy(numDom = numDom.assume(cond))
        else
          this.assume(left).assume(right)
      case BinaryBooleanExpression(left, right, BooleanOperator.||, typ) =>
        if (cond.canonical) // return the current state with updated numDom
          this.copy(numDom = numDom.assume(cond))
        else
          this.assume(left) lub this.assume(right)

      // NegatedBooleanExpression
      case cond:NegatedBooleanExpression => {
        cond.exp match {
          // Constant
          case c: Constant => // return the current state with updated numDom
            this.copy(numDom = numDom.assume(cond))

          // Identifier (i.e., FieldIdentifier, VariableIdentifier)
          case id: Identifier => // return the current state with updated numDom
            this.copy(numDom = numDom.assume(cond))

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

      case ReferenceComparisonExpression(left, right, op, typ) =>
        if (op == ArithmeticOperator.==) {
          println(left.getClass.getSimpleName, left.typ)
          println(right.getClass.getSimpleName, right.typ)
          this
        } else { // cond.op == ArithmeticOperator.!=
          println(left.getClass.getSimpleName, left.typ)
          println(right.getClass.getSimpleName, right.typ)
          this
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

    // return a new state with bottom exprSet and empty refToObj map
    PermissionState(exprSet.bottom(),refToObj.empty,objFieldToObj.empty,numDom.bottom())
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
    
    val obj = HeapIdentifier(typ,pp) // create new Obj
    val objFieldToObjmap = objFieldToObj + (obj -> Map[String,Set[HeapIdentifier]]()) // add key to objFieldToObj map
    // return the current state with updated exprSet and updated objFieldToObj map
    this.copy(exprSet = ExpressionSet(obj), objFieldToObj = objFieldToObjmap)
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

    if (typ.isObject) { // the variable to be created is a `Ref`
      val refToObjmap = refToObj + (x -> Set[HeapIdentifier]()) // add key to refToObj map
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
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): PermissionState = {
    logger.debug("*** createVariableForArgument(" + x.toString + "; " + typ.toString + ")")

    if (typ.isObject) { // the variable to be created is a `Ref`
      val obj = HeapIdentifier(typ,x.pp) // create new Obj
      val refToObjmap = refToObj + (x -> Set[HeapIdentifier](obj)) // add key to refToObj map
      val objFieldToObjmap = objFieldToObj + (obj -> Map[String,Set[HeapIdentifier]]()) // add key to objFieldToObj map
      // return the current state with updated exprSet, updated refToObj and updated objFieldToObj
      this.copy(exprSet = ExpressionSet(x), refToObj = refToObjmap, objFieldToObj = objFieldToObjmap)
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
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** evalConstant(" + value + "; " + typ.toString + "; " + pp.toString + ")")

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
  private def evaluatePath(path: List[String], objFieldToObjmap: Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]]) : Set[HeapIdentifier] = {
    val keys = refToObj.keySet // set of all Ref variables
    // retrieving the Ref variable corresponding to the head of the path
    val id = keys.find((ref) => ref.name == path.head).get
    val fst = refToObj(id) // set of objects pointed by the Ref variable
    // path evaluation
    path.drop(1).dropRight(1).foldLeft(fst)(
      (set,next) => // next path segment
        set.foldLeft(Set[HeapIdentifier]())(
          (s,obj) => s ++ objFieldToObjmap.get(obj).get.get(next).get
        )
    )
  }

  /** The current expression.
    *
    * Invoked after each statement to retrieve its result.
    */
  override def expr: ExpressionSet = {
    logger.debug("*** expr: " + this.exprSet.toString)
    
    this.exprSet // return exprSet
  }

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    *
    * @todo implement me!
    */
  override def factory(): PermissionState = {
    logger.debug("*** factory(): implement me!")
    
    this
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
        val path = obj.stringPath // path to evaluate
        // update objFieldToObj map
        val segment = if (typ.isObject) path.drop(1) else path.drop(1).dropRight(1)
        val objFieldToObjmap = segment.foldLeft(objFieldToObj)(
          (map,next) =>
            map.mapValues[Map[String,Set[HeapIdentifier]]](
              (m) => if (m.contains(next)) m else m + (next -> Set[HeapIdentifier]())
            )
        )
        // evaluate path into the set of objects referenced by it (up to the given field excluded)
        val objSet = evaluatePath(path,objFieldToObjmap)
        val expr = objSet.foldLeft(ExpressionSet())(
          (e,o) => {
            val fld = FieldIdentifier(o,field,typ) // create new FieldIdentifier
            e add ExpressionSet(fld)
          }
        )
        // return the current state with updated exprSet, updated objFieldToObj
        this.copy(exprSet = expr, objFieldToObj = objFieldToObjmap)
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

    // return the current state with updated exprSet
    this.copy(exprSet = ExpressionSet(id))
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

    false
  }

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return `true` if and only if the state is equivalent to top
    *
    * @todo implement me!
    */
  override def isTop: Boolean = {
    logger.debug("*** isTop: implement me!")
    
    false
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return `true` if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: PermissionState): Boolean = {
    logger.debug("*** lessEqual(" + other.repr + ")")

    val exp = this.exprSet.lessEqual(other.exprSet) // test the exprSets
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
    exp && refToObjmap && objFieldToObjmap && num
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: PermissionState): PermissionState = {
    logger.debug("*** lub(" + other.repr + ")")

    val exp = this.exprSet lub other.expr // join the exprSets
    val refToObjmap = this.refToObj ++ other.refToObj.map {
      case (k: VariableIdentifier,v: Set[HeapIdentifier]) => k -> (this.refToObj.getOrElse(k,Set[HeapIdentifier]()) ++ v)
    } // merge the refToObjs
    val objFieldToObjmap = this.objFieldToObj ++ other.objFieldToObj.map {
      case (o: HeapIdentifier,m: Map[String,Set[HeapIdentifier]]) => o ->
        (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()) ++ other.objFieldToObj(o).map {
          case (s: String, v: Set[HeapIdentifier]) => s ->
            (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()).getOrElse(s,Set[HeapIdentifier]()) ++ v)
        })
    } // merge the objFieldToObjmap
    val num = this.numDom lub other.numDom // join the numDoms

    // return the current state with updated exprSet, updated refToObj, updated objFieldToObjmap and updated numDom
    this.copy(exprSet = exp, refToObj = refToObjmap, objFieldToObj = objFieldToObjmap, numDom = num)
  }

  /** Performs abstract garbage collection.
    *
    * @todo implement me!
    */
  override def pruneUnreachableHeap(): PermissionState = {
    logger.debug("*** pruneUnreachableHeap(): implement me!")
    this
  }

  /** Removes all variables satisfying filter.
    *
    * @todo implement me!
    */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): PermissionState = {
    logger.debug("*** pruneVariables(" + filter.toString + "): implement me!")
    
    this
  }

  /** Removes the current expression.
    *
    * @return The abstract state obtained after removing the current expression
    */
  override def removeExpression(): PermissionState = {
    logger.debug("*** removeExpression()")

    // return the current state with a new exprSet
    this.copy(exprSet = ExpressionSet())
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
    
    this
  }

  /** The default state string representation.
    *
    * @return the default string representation of the current state
    */
  private def repr: String = {
    "PermissionState(" +
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
    *
    * @todo implement me!
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): PermissionState = {
    logger.debug("*** setArgument(" + x.toString + "; " + right.toString + "): implement me!")
    
    this
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
    
    this.copy(exprSet = expr) // return the current state with updated exprSet
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
    
    this
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
    
    this
  }

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    *
    * @todo implement me!
    */
  override def top(): PermissionState = {
    logger.debug("*** top(): implement me!")
    
    this
  }

  /** The state string representation.
    *
    * @return the string representation of the current state
    */
  override def toString(): String = {
    "PermissionState(\n" +
    "\texprSet: " + exprSet.toString + "\n" +
    "\trefToObj: " + refToObj.toString + "\n" +
    "\tobjFieldToObj: " + objFieldToObj.toString + "\n" +
    "\tnumDom: " + numDom.toString + "\n" +
    ")"
  }

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    *
    * @todo implement me!
    */
  override def widening(other: PermissionState): PermissionState = {
    logger.debug("*** widening(" + other.repr + ")")

    val exp = this.exprSet widening other.expr // widen the exprSets
    val refToObjmap = this.refToObj ++ other.refToObj.map {
        case (k: VariableIdentifier,v: Set[HeapIdentifier]) => k -> (this.refToObj.getOrElse(k,Set[HeapIdentifier]()) ++ v)
      } // merge the refToObjs
    val objFieldToObjmap = this.objFieldToObj ++ other.objFieldToObj.map {
        case (o: HeapIdentifier,m: Map[String,Set[HeapIdentifier]]) => o ->
          (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()) ++ other.objFieldToObj(o).map {
            case (s: String, v: Set[HeapIdentifier]) => s ->
              (this.objFieldToObj.getOrElse(o,Map[String,Set[HeapIdentifier]]()).getOrElse(s,Set[HeapIdentifier]()) ++ v)
          })
      } // merge the objFieldToObjmap
    val num = this.numDom widening other.numDom // widen the numDoms

    // return the current state with updated exprSet, updated refToObj, updated objFieldToObjmap and updated numDom
    this.copy(exprSet = exp, refToObj = refToObjmap, objFieldToObj = objFieldToObjmap, numDom = num)
  }
}


/** Builds permission analysis entry states for given method declarations. */
object PermissionEntryStateBuilder extends EntryStateBuilder[PermissionState] {
  override def topState: PermissionState = PermissionState(ExpressionSet(),Map[VariableIdentifier,Set[HeapIdentifier]](),
    Map[HeapIdentifier,Map[String,Set[HeapIdentifier]]](),Apron.Polyhedra.Bottom.factory())
}

class PermissionAnalysis extends SimpleAnalysis[PermissionState](PermissionEntryStateBuilder) {
  override def analyze(method: MethodDeclaration): AnalysisResult[PermissionState] = {
    val result = analyze(method, entryStateBuilder.build(method))
    println("Analysis Result:\n" + result)
    result
  }
}

object PermissionAnalysisRunner extends SilAnalysisRunner[PermissionState] {
  //val analysis = SimpleAnalysis[PermissionState](PermissionEntryStateBuilder)
  val analysis = new PermissionAnalysis

  override def toString = "Stupid Analysis"
}

