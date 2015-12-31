package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
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
  * @param obj the `Obj` to turn into an Expression
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
  */
case class Fld(obj: Obj, field: String) {

  /** The name of the object field. */
  def name : String = obj.name + "." + field

  override def equals(o: Any) = o match {
    case that: Obj => that.name.equals(this.name)
    case _ => false
  }
  override def hashCode = name.hashCode
}

/** Permission Inference State
  *
  * Note that each abstract state must include an `ExpressionSet`!
  * It is accessed during the analysis to retrieve the result of each statement!!
  *
  * @author Caterina Urban
  */
case class PermissionState(exprSet: ExpressionSet,
                           refToObj: Map[Ref,Set[Obj]],
                           objFieldToObj: Map[Obj,Map[String,Set[Obj]]])
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
    *
    * @todo implement me!
    */
  override def assignField(obj: Expression, field: String, right: Expression): PermissionState = {
    logger.debug("*** assignField(obj: " + obj.toString + ", field: " + field.toString + ", right: " + right.toString + "): implement me!")
    
    this
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    *
    * @todo fully implement me!
    */
  override def assignVariable(x: Expression, right: Expression): PermissionState = {
    logger.debug("*** assignVariable(x: " + x.toString + ", right: " + right.toString + "): fully implement me!")

    // warning: handling Ref variable assignment to Object only
    {
      x match {
        case x: Identifier =>
          val xref = Ref(x) // create new Ref
          right match {
              case right: ObjExpression => // `x = new()`
                val rightobj = Obj(right.typ,right.pp) // create new Obj
                // add xref -> rightobj to refToObj map
                val refToObjmap = refToObj + (xref -> Set[Obj](rightobj))
                // return the current state with updated refToObj
                this.copy(refToObj = refToObjmap)
              case right: Identifier => // `x = y`
                val rightref = Ref(right)
                // add xref -> refToObj[rightref] to refToObj map
                val refToObjmap = refToObj + (xref -> this.refToObj.getOrElse(rightref,Set[Obj]()))
                // return the current state with updated refToObj
                this.copy(refToObj = refToObjmap)
              case _ => this // TODO
            }
        case _ => this // TODO
      }
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
    logger.debug("*** assume(cond :" + cond.toString + "): implement me!")
    
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
    logger.debug("*** before: " + pp.toString)

    this  // return the current state without modification
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): PermissionState = {
    logger.debug("*** bottom()")

    // return a new state with bottom exprSet and empty refToObj map
    PermissionState(exprSet.bottom(),refToObj.empty,objFieldToObj.empty)
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
    logger.debug("*** createObject(typ: " + typ.toString + ", pp: " + pp.toString + ")")
    
    val obj = Obj(typ,pp) // create new Obj
    val exp = ObjExpression(obj) // turn Obj into Expression
    this.copy(exprSet = ExpressionSet(exp)) // return the current state with updated exprSet
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
    *
    * @todo fully implement me!
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** createVariable(x: " + x.toString + ", typ: " + typ.toString + ", pp: " + pp.toString + "): fully implement me!")
    
    // warning: handling Ref variable declarations only
    if (typ.isObject) {
      val ref = Ref(x) // create new Ref
      val refToObjmap = refToObj + (ref -> Set[Obj]()) // add key to refToObj map
      val exp = RefExpression(ref) // turn Ref into Expression
      this.copy(exprSet = ExpressionSet(exp), refToObj = refToObjmap) // return the current state with updated exprSet and refToObj
    } else {
      this // TODO
    }
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    *
    * @todo implement me!
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): PermissionState = {
    logger.debug("*** createVariableForArgument(x: " + x.toString + ", typ: " + typ.toString + "): implement me!")
    
    this
  }

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ The type of the numerical constant
    * @param pp The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    *
    * @todo implement me!
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): PermissionState = {
    logger.debug("*** evalConstant(value: " + value + ", typ: " + typ.toString + ", pp: " + pp.toString + "): implement me!")
    
    this
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

  def evaluatePath(obj: AccessPathIdentifier) : Set[Obj] = {
    val path = obj.stringPath

    //foldLeft[B](z: B)(f: (B, A) ⇒ B): B

    val keys = refToObj.keySet
    val id = keys.find((ref) => ref.name == path.head)


    val fst = refToObj(path.head)
    path.foldLeft(fst)(
      (s,f) => s.map
    )(path.tail)

    Set[Obj]()
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object on which the field access is performed
    * @param field the name of the field
    * @param typ the type of the field
    * @return The abstract state obtained after the field access, that is,
    *         a new state whose `ExpressionSet` holds the symbolic representation of the value of the given field.
    *
    * @todo implement me!
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): PermissionState = {
    logger.debug("*** getFieldValue(obj: " + obj.toString + ", field: " + field + ", typ: " + typ.toString + "): implement me!")
    obj match {
      case obj:AccessPathIdentifier =>
        //val id = VariableIdentifier
        //val ref = Ref()
        //val obj = Obj(Ref,obj.pp)
        //val fld = Fld()
        println(obj.objName)
      case _ => println("something else")
    }
    this
  }

  /** Gets the value of a variable.
    *
    * Invoked by variable declarations (`var x : Ref`) and variable assignments (`x = ...`, ...)
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    *
    * @todo fully implement me!
    */
  override def getVariableValue(id: Identifier): PermissionState = {
    logger.debug("*** getVariableValue(id : " + id.toString + "): fully implement me!")

    id match {
      case _: AccessPathIdentifier => println("AccessPathIdentifier")
      case _: VariableIdentifier => println("VariableIdentifier")
      case _ => println("Unknown")
    }

    // warning: handling Ref variable declarations only
    if (id.typ.isObject) { // the variable is of type Ref
      // handling variable declarations `var x : Ref` and variable assignments `x = ...`
      this.copy(exprSet = ExpressionSet(id)) // return the current state with updated exprSet
    } else {
      this // TODO
    }
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
    logger.debug("*** glb(other: " + other.toString + "): implement me!")
    
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
    logger.debug("*** lessEqual(other: " + other.toString + ")")

    val exp = this.exprSet.lessEqual(other.exprSet) // test the exprSets
    val refToObjmap = this.refToObj.forall {
      case (k : Ref,v : Set[Obj]) => v subsetOf this.refToObj.getOrElse(k,Set[Obj]())
    } // test the refToObjs
    exp && refToObjmap
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: PermissionState): PermissionState = {
    logger.debug("*** lub(other: " + other.toString + ")")

    val exp = this.exprSet.lub(other.expr) // join the exprSets
    val refToObjmap = this.refToObj ++ other.refToObj.map {
      case (k : Ref,v : Set[Obj]) => k -> (this.refToObj.getOrElse(k,Set[Obj]()) ++ v)
    } // merge the refToObjs
    this.copy(exprSet = exp, refToObj = refToObjmap) // return the current state with updated exprSet and refToObj
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
    logger.debug("*** pruneVariables(filter: " + filter.toString + "): implement me!")
    
    this
  }

  /** Removes the current expression.
    *
    * @return The abstract state obtained after removing the current expression
    */
  override def removeExpression(): PermissionState = {
    logger.debug("*** removeExpression()")
    
    this.copy(exprSet = ExpressionSet()) // return the current state with a new exprSet
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
    logger.debug("*** removeVariable(varExpr: " + varExpr.toString + "): implement me!")
    
    this
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
    logger.debug("*** setArgument(other: " + x.toString + ", right: " + right.toString + "): implement me!")
    
    this
  }

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): PermissionState = {
    logger.debug("*** setExpression(expr: " + expr.toString + ")")
    
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
    logger.debug("*** setVariableToTop(varExpr: " + varExpr.toString + "): implement me!")
    
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
    logger.debug("*** throws(t: " + t.toString + "): implement me!")
    
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

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    *
    * @todo implement me!
    */
  override def widening(other: PermissionState): PermissionState = {
    logger.debug("*** widening(other: " + other.toString + "): implement me!")
    
    this
  }
}

object PermissionAnalysisRunner extends SilAnalysisRunner[PermissionState] {
  val analysis = SimpleAnalysis[PermissionState](PermissionEntryStateBuilder)

  override def toString = "Stupid Analysis"
}

object PermissionEntryStateBuilder extends EntryStateBuilder[PermissionState] {
  override def topState: PermissionState =
    PermissionState(ExpressionSet(),Map[Ref,Set[Obj]](),Map[Obj,Map[String,Set[Obj]]]())
}