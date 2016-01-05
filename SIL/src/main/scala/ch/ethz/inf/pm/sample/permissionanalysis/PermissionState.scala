package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.execution.{EntryStateBuilder, SimpleAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.sil.SilAnalysisRunner
import ch.ethz.inf.pm.sample.oorepresentation.{LineColumnProgramPoint, ProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging

/** Reference variable
  *
  * @param id the identifier corresponding to the variable
  * @author Caterina Urban
  */
case class Ref(id: Identifier) {
  def name: String = id.getName

  override def equals(o: Any) = o match {
    case that: Ref => that.name.equals(this.name)
    case _ => false
  }
  override def hashCode = name.hashCode
}

/** Wrapper that turns a `Ref` into an `Expression`
  *
  * @param ref the `Ref` to turn into an `Expression`
  * @author Caterina Urban
  */
case class RefExpression(ref: Ref) extends Expression {

  /** The type of the expression. */
  override def typ: Type = ref.id.typ
  /** Point in the program where the expression is located. */
  override def pp: ProgramPoint = ref.id.pp
  /** The string representation of the expression. */
  override def toString : String = ref.name

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = ???
  /** Runs `f` on the expression and all sub-expressions. */
  override def transform(f: (Expression) => Expression): Expression = f(this)
  /** Checks if `f` evaluates to `true` on the expression and all sub-expressions. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this)

}

/** Object created at object allocation site.
  *
  * @param typ the type of the object
  * @param pp the object allocation site
  * @author Caterina Urban
  */
case class Obj(typ: Type, pp: ProgramPoint) {

  /** The name of the object. */
  def name : String = "O" + number

  def number : String = pp match {
    case pp:LineColumnProgramPoint => pp.getLine.toString
    case _ => pp.description
  }

  override def toString : String = name

  override def equals(o: Any) = o match {
    case that: Obj => that.name.equals(this.name)
    case _ => false
  }
  override def hashCode = name.hashCode
}

/** Wrapper that turns an `Obj` into an `Expression`
  *
  * @param obj the `Obj` to turn into an `Expression`
  * @author Caterina Urban
  */
case class ObjExpression(obj: Obj) extends Expression {

  /** The type of the expression. */
  override def typ: Type = obj.typ
  /** Point in the program where the expression is located. */
  override def pp: ProgramPoint = obj.pp
  /** The string representation of the expression. */
  override def toString : String = obj.name

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = ???
  /** Runs `f` on the expression and all sub-expressions. */
  override def transform(f: (Expression) => Expression): Expression = f(this)
  /** Checks if `f` evaluates to `true` on the expression and all sub-expressions. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this)

}

/** Field of an object.
  *
  * @param obj the object
  * @param field the field name
  * @author Caterina Urban
  */
case class Fld(obj: Obj, field: String, typ: Type) {

  /** The name of the object field. */
  def name : String = obj.name + "." + field

  override def equals(o: Any) = o match {
    case that: Obj => that.name.equals(this.name)
    case _ => false
  }
  override def hashCode = name.hashCode

}

/** Wrapper that turns a `Fld` into an `Expression`
  *
  * @param fld the `Fld` to turn into an `Expression`
  * @author Caterina Urban
  */
case class FldExpression(fld: Fld) extends Expression {

  /** The type of the expression. */
  override def typ: Type = fld.typ
  /** Point in the program where the expression is located. */
  override def pp: ProgramPoint = fld.obj.pp
  /** The string representation of the expression. */
  override def toString : String = fld.name

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = ???
  /** Runs `f` on the expression and all sub-expressions. */
  override def transform(f: (Expression) => Expression): Expression = f(this)
  /** Checks if `f` evaluates to `true` on the expression and all sub-expressions. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this)

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
                           refToObj: Map[Ref,Set[Obj]],
                           // map from heap `Obj` objects to a map from `Ref` fields to heap `Obj` objects
                           objFieldToObj: Map[Obj,Map[String,Set[Obj]]],
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
      case obj: FldExpression =>
        if (obj.fld.typ.isObject) { // the assigned field is a `Ref`
          right match {

            case right: FldExpression => // e.g., `x.f := y.g`
              val s = objFieldToObj(right.fld.obj)(right.fld.field) // retrieve the heap `Obj` objects
              val o = obj.fld.obj // retrieve `Obj` whose field is assigned
              val f = obj.fld.field // retrieve assigned field
              // weak update
              val objFieldToObjmap = objFieldToObj + (o -> (objFieldToObj(o) + (f -> (objFieldToObj(o)(f) ++ s))))
              // return the current state with updated objFieldToObj
              this.copy(objFieldToObj = objFieldToObjmap)

            case right: ObjExpression => // e.g., `x.f := new()`
              val o = obj.fld.obj // retrieve `Obj` whose field is assigned
              val f = obj.fld.field // retrieve assigned field
              val objFieldToObjmap = objFieldToObj +
                // add key to objFieldToObjmap
                (right.obj -> Map[String,Set[Obj]]()) +
                // weak update
                (o -> (objFieldToObj(o) + (f -> (objFieldToObj(o)(f) + right.obj))))
              // return the current state with updated objFieldToObj
              this.copy(objFieldToObj = objFieldToObjmap)

            case right: VariableIdentifier => // e.g., `x.f := y`
              val id = Ref(right) // create `Ref` variable
              val s = refToObj(id) // retrieve the corresponding heap `Obj` objects
              val o = obj.fld.obj // retrieve `Obj` whose field is assigned
              val f = obj.fld.field // retrieve assigned field
              // weak update
              val objFieldToObjmap = objFieldToObj + (o -> (objFieldToObj(o) + (f -> (objFieldToObj(o)(f) ++ s))))
              // return the current state with updated objFieldToObj
              this.copy(objFieldToObj = objFieldToObjmap)

            case _ => throw new NotImplementedError("A field assignment implementation is missing.")
          }
        } else {  // the assigned field is not a `Ref`
          right match {
            case right : FldExpression => // e.g., `x.f := y.g`
              val id = VariableIdentifier(obj.fld.name)(obj.fld.typ,obj.pp)
              val rhs = VariableIdentifier(right.fld.name)(right.fld.typ,right.pp)
              // weak update
              val num = numDom.lub(numDom.assign(id,rhs))
              // return the current state with updated numDom
              this.copy(numDom = num)

            case _ =>
              val id = VariableIdentifier(obj.fld.name)(obj.fld.typ,obj.pp)
              // weak update
              val num = numDom.lub(numDom.assign(id,right))
              // return the current state with updated numDom
              this.copy(numDom = num)
          }
        }
      case _ => throw new IllegalArgumentException("A field assignment must occur via a FldExpression.")
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
      case _: FldExpression => "FldExpression"
      case _: ObjExpression => "ObjExpression"
      case _: VariableIdentifier => "VariableIdentifier"
      case _ => "Unknown: " + right.getClass.getSimpleName
    }
    logger.debug("*** assignVariable(" + x.toString + ": " + idx + "; " + right.toString + ": " + idright + ")")

    x match {
      case x: VariableIdentifier =>
        if (x.typ.isObject) { // the assigned variable is a `Ref`
          val xref = Ref(x) // create new `Ref` variable
          right match {
            case right: FldExpression => // e.g., `x := y.g`
              val s = this.objFieldToObj(right.fld.obj)(right.fld.field) // retrieve the heap `Obj` objects
              // add xref -> s to refToObj map
              val refToObjmap = this.refToObj + (xref -> s)
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap)

            case right: ObjExpression => // e.g., `x = new()`
              val rightobj = Obj(right.typ, right.pp) // create new Obj
              // add xref -> rightobj to refToObj map
              val refToObjmap = this.refToObj + (xref -> Set[Obj](rightobj))
              // add key to objFieldToObj map
              val objFieldToObjmap = this.objFieldToObj + (rightobj -> Map[String,Set[Obj]]())
              // return the current state with updated refToObj and updated objFieldToObj
              this.copy(refToObj = refToObjmap, objFieldToObj = objFieldToObjmap)

            case right: VariableIdentifier => // e.g., `x := y`
              val rightref = Ref(right)
              // add xref -> refToObj[rightref] to refToObj map
              val refToObjmap = this.refToObj + (xref -> this.refToObj.getOrElse(rightref, Set[Obj]()))
              // return the current state with updated refToObj
              this.copy(refToObj = refToObjmap)

            case _ => throw new NotImplementedError("A variable assignment implementation is missing.")
          }
        } else {  // the assigned variable is not a `Ref`
          right match {
            case right : FldExpression => // e.g., `x := y.g`
              val id = VariableIdentifier(right.fld.name)(right.fld.typ,right.pp)
              // return the current state with updated numDom
              this.copy(numDom = numDom.assign(x,id))

            case _ =>
              // return the current state with updated numDom
              this.copy(numDom = numDom.assign(x,right))
          }
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
    *
    * @todo implement me!
    */
  override def assume(cond: Expression): PermissionState = {
    logger.debug("*** assume(" + cond.toString + "): implement me!")
    
    this
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
    
    val obj = Obj(typ,pp) // create new Obj
    val objFieldToObjmap = objFieldToObj + (obj -> Map[String,Set[Obj]]()) // add key to objFieldToObj map
    val exp = ObjExpression(obj) // turn Obj into Expression
    // return the current state with updated exprSet and updated objFieldToObj map
    this.copy(exprSet = ExpressionSet(exp), objFieldToObj = objFieldToObjmap)
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
      val ref = Ref(x) // create new Ref
      val refToObjmap = refToObj + (ref -> Set[Obj]()) // add key to refToObj map
      val exp = RefExpression(ref) // turn Ref into Expression
      // return the current state with updated exprSet and refToObj
      this.copy(exprSet = ExpressionSet(exp), refToObj = refToObjmap)
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
      val obj = Obj(typ,x.pp) // create new Obj
      val ref = Ref(x) // create new Ref
      val refToObjmap = refToObj + (ref -> Set[Obj](obj)) // add key to refToObj map
      val objFieldToObjmap = objFieldToObj + (obj -> Map[String,Set[Obj]]()) // add key to objFieldToObj map
      val exp = RefExpression(ref) // turn Ref into Expression
      // return the current state with updated exprSet, updated refToObj and updated objFieldToObj
      this.copy(exprSet = ExpressionSet(exp), refToObj = refToObjmap, objFieldToObj = objFieldToObjmap)
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
  private def evaluatePath(path: List[String], objFieldToObjmap: Map[Obj,Map[String,Set[Obj]]]) : Set[Obj] = {
    val keys = refToObj.keySet // set of all Ref variables
    // retrieving the Ref variable corresponding to the head of the path
    val id = keys.find((ref) => ref.name == path.head).get
    val fst = refToObj(id) // set of objects pointed by the Ref variable
    // path evaluation
    path.drop(1).dropRight(1).foldLeft(fst)(
      (set,next) => // next path segment
        set.foldLeft(Set[Obj]())(
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
    val idobj = obj match {
      case _: AccessPathIdentifier => "AccessPathIdentifier"
      case _ => "Unknown"
    }
    logger.debug("*** getFieldValue(" + obj.toString + ": " + idobj + "; " + field + "; " + typ.toString + ")")

    obj match {
      case obj:AccessPathIdentifier =>
        val path = obj.stringPath // path to evaluate
        // update objFieldToObj map
        val segment = if (typ.isObject) path.drop(1) else path.drop(1).dropRight(1)
        val objFieldToObjmap = segment.foldLeft(objFieldToObj)(
          (map,next) =>
            map.mapValues[Map[String,Set[Obj]]](
              (m) => if (m.contains(next)) m else m + (next -> Set[Obj]())
            )
        )
        // evaluate path into the set of objects referenced by it (up to the given field excluded)
        val objSet = evaluatePath(path,objFieldToObjmap)
        val expr = objSet.foldLeft(ExpressionSet())(
          (e,o) => {
            val fld = FldExpression(Fld(o,field,typ)) // create new FldExpression
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
    val typ = id match {
      case _: VariableIdentifier => "VariableIdentifier"
      case _ => "Unknown"
    }
    logger.debug("*** getVariableValue(" + id.toString + ": " + typ + ")")

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
      case (k: Ref,v: Set[Obj]) => v subsetOf other.refToObj.getOrElse(k,Set[Obj]())
    } // test the refToObjs
    val objFieldToObjmap = this.objFieldToObj.forall {
      case (o: Obj, m: Map[String,Set[Obj]]) => m.forall {
        case (f: String, s: Set[Obj]) =>
          s subsetOf other.objFieldToObj.getOrElse(o,Map[String,Set[Obj]]()).getOrElse(f,Set[Obj]())
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

    val exp = this.exprSet.lub(other.expr) // join the exprSets
    val refToObjmap = this.refToObj ++ other.refToObj.map {
      case (k: Ref,v: Set[Obj]) => k -> (this.refToObj.getOrElse(k,Set[Obj]()) ++ v)
    } // merge the refToObjs
    val objFieldToObjmap = this.objFieldToObj ++ other.objFieldToObj.map {
      case (o: Obj,m: Map[String,Set[Obj]]) => o ->
        (this.objFieldToObj.getOrElse(o,Map[String,Set[Obj]]()) ++ other.objFieldToObj(o).map {
          case (s: String, v: Set[Obj]) => s ->
            (this.objFieldToObj.getOrElse(o,Map[String,Set[Obj]]()).getOrElse(s,Set[Obj]()) ++ v)
        })
    } // merge the objFieldToObjmap
    val num = this.numDom.lub(other.numDom) // join the numDoms

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
    logger.debug("*** widening(" + other.repr + "): implement me!")
    
    this
  }
}

object PermissionAnalysisRunner extends SilAnalysisRunner[PermissionState] {
  val analysis = SimpleAnalysis[PermissionState](PermissionEntryStateBuilder)

  override def toString = "Stupid Analysis"
}

object PermissionEntryStateBuilder extends EntryStateBuilder[PermissionState] {
  override def topState: PermissionState = PermissionState(ExpressionSet(),
    Map[Ref,Set[Obj]](),Map[Obj,Map[String,Set[Obj]]](),Apron.Polyhedra.Bottom.factory())
}