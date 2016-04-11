package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron.Polyhedra
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{Apron, BoxedNonRelationalNumericalDomain, DoubleInterval, NumericalDomain}
import ch.ethz.inf.pm.sample.execution.{EntryStateBuilder, SimpleAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilAnalysisRunner
import ch.ethz.inf.pm.sample.oorepresentation._
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
  * @param obj the received heap node
  * @param field the field name
  * @param typ the field type
  * @param pp the access program point
  * @author Caterina Urban
  */
case class FieldAccess(obj: HeapNode, field: String, typ: Type, pp: ProgramPoint) extends Identifier {
  override def getName: String = obj.getName + "." + field
  override def getField: Option[String] = Some(field)
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
  extends SimpleState[S] with StateWithBackwardAnalysisStubs[S] with LazyLogging
{
  this: S =>

  def fieldSet: Set[(Type,String)] // fields declared within the program

  def currentPP: ProgramPoint // current program point
  def nonce: Int // freshly generated heap node

  def exprSet: ExpressionSet // result of previous statement
  // map from Ref variables to heap objects
  def refToObj: Map[VariableIdentifier,Set[HeapNode]]
  // map from heap objects to a map from Ref fields to heap objects
  def objFieldToObj: Map[HeapNode,Map[String,Set[HeapNode]]]
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
    logger.debug("*** ----------------assignField(" + obj.toString + "; " + field.toString + "; " + right.toString + ")")

    ???
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

    ???
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

    ???
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
    val nonce = 0
    // return a new state with bottom exprSet, empty refToObj, empty objFieldToObj, bottom numDom
    val expr = exprSet.bottom()
    val refToObj = Map[VariableIdentifier,Set[HeapNode]]()
    val objFieldToObj = Map[HeapNode,Map[String,Set[HeapNode]]]()
    val num = numDom.bottom()
    this.copy(fields,currentPP,nonce,expr,refToObj,objFieldToObj,num)
  }

  def copy(fieldSet: Set[(Type, String)] = fieldSet,
           currentPP: ProgramPoint = currentPP,
           nonce: Int = nonce,
           exprSet: ExpressionSet = exprSet,
           refToObj: Map[VariableIdentifier, Set[HeapNode]] = refToObj,
           objFieldToObj: Map[HeapNode, Map[String, Set[HeapNode]]] = objFieldToObj,
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
    logger.debug("*** ----------------createVariable(" + x.toString + "; " + typ.toString + "; " + pp.toString + ")")

    ???
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

    ???
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
    val nonce = 0
    // return a new state with factory exprSet, empty refToObj, empty objFieldToObj, factory numDom
    val expr = ExpressionSet()
    val refToObj = Map[VariableIdentifier,Set[HeapNode]]()
    val objFieldToObj = Map[HeapNode,Map[String,Set[HeapNode]]]()
    val num = numDom.factory()
    this.copy(fields,currentPP,nonce,expr,refToObj,objFieldToObj,num)
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

    ???
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
    this.copy(exprSet = ExpressionSet(id))  // return the current state with updated exprSet
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

    ???
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: S): S = {
    logger.debug("*** lub(" + this.repr + ", " + other.repr + ")")

    ???
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): S = {
    //logger.debug("*** pruneUnreachableHeap()")

    ???
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
    val objFieldToObj = Map[HeapNode,Map[String,Set[HeapNode]]]()
    val num = numDom.top()
    this.copy(fields,currentPP,nonce,expr,refToObj,objFieldToObj,num)
  }

  /** The state string representation.
    *
    * @return the string representation of the current state
    */
  override def toString: String = {
    "MayPointToNumericalState(\n" +
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
    */
  override def widening(other: S): S = {
    logger.debug("*** ----------------widening(" + other.repr + ")")

    ???
  }
}

/** MayPointTo+Polyhedra Analysis State.
  *
  * @param fieldSet fields declared within the program
  * @param currentPP current program point
  * @param nonce freshly generated heap node
  * @param exprSet result of previous statement
  * @param refToObj map from Ref variables to heap objects
  * @param objFieldToObj map from heap objects to a map from Ref fields to heap objects
  * @param numDom numerical abstract domain
  * @author Caterina Urban
  */
case class MayPointToPolyhedraState(fieldSet: Set[(Type, String)],
                                    currentPP: ProgramPoint,
                                    nonce: Int,
                                    exprSet: ExpressionSet,
                                    refToObj: Map[VariableIdentifier, Set[HeapNode]],
                                    objFieldToObj: Map[HeapNode, Map[String, Set[HeapNode]]],
                                    numDom: Apron.Polyhedra)
  extends MayPointToNumericalState[Apron.Polyhedra,MayPointToPolyhedraState] {
  override def copy(fieldSet: Set[(Type, String)],
                    currentPP: ProgramPoint,
                    nonce: Int,
                    exprSet: ExpressionSet,
                    refToObj: Map[VariableIdentifier, Set[HeapNode]],
                    objFieldToObj: Map[HeapNode, Map[String, Set[HeapNode]]],
                    numDom: Polyhedra): MayPointToPolyhedraState =
    MayPointToPolyhedraState(fieldSet, currentPP, nonce, exprSet, refToObj, objFieldToObj, numDom)
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

  override def topState = MayPointToPolyhedraState(fields, DummyProgramPoint, 0,
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
