/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.analysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.domain.HeapNode.NewNode
import ch.ethz.inf.pm.sample.domain.{AliasDomain, MayAliasGraph, MustAliasGraph}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.{Compilable, DummyProgramPoint, Type}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverAnalysisRunner, SilverMethodDeclaration, SilverProgramDeclaration}
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.{Expression, ProgramPoint}
import ch.ethz.inf.pm.sample.permissionanalysis.util.Context
import com.typesafe.scalalogging.LazyLogging

/**
  * A state of the alias analysis.
  *
  * @tparam T The type of the alias analysis state.
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait AliasAnalysisState[T <: AliasAnalysisState[T, May, Must], May <: AliasDomain[May, _], Must <: AliasDomain[Must, _]]
  extends SilverState[T]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {

  this: T =>

  /**
    * Returns the element of the may alias domain stored in the state.
    *
    * @return The element of the may alias domain.
    */
  def may: May

  /**
    * Returns the element of the must alias domain stored in the state.
    *
    * @return The element of the must alias domain.
    */
  def must: Must

  /**
    * Returns the current program point.
    *
    * @return The current program point.
    */
  def pp: ProgramPoint

  /**
    * Creates an alias analysis state with the given fields.
    *
    * @param fields The fields.
    * @return The alias analysis state.
    */
  def factory(fields: Seq[Identifier]): T = copy(
    isTop = false,
    isBottom = false,
    may = may.factory(fields),
    must = must.factory(fields),
    expr = ExpressionSet(),
    pp = DummyProgramPoint
  )

  /* ------------------------------------------------------------------------- *
   * LATTICE METHODS
   */

  override def factory(): T = top()

  override def top(): T = copy(isTop = true, isBottom = false)

  override def bottom(): T = copy(isTop = false, isBottom = true)

  override def lub(other: T): T = {
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else {
      // least upper bound of may and must alias graphs
      val newMay = may lub other.may
      val newMust = must lub other.must
      // update state
      copy(
        may = newMay,
        must = newMust,
        expr = ExpressionSet(),
        pp = DummyProgramPoint
      )
    }
  }

  override def glb(other: T): T = {
    if (isBottom || other.isTop) this
    else if (isTop || other.isBottom) other
    else {
      // greatest lower bound of may and must alias graphs
      val newMay = may glb other.may
      val newMust = must glb other.must
      // update state
      copy(
        may = newMay,
        must = newMust,
        expr = ExpressionSet(),
        pp = DummyProgramPoint
      )
    }
  }

  override def widening(other: T): T = {
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else {
      // widening of may and must alias graphs
      val newMay = may widening other.may
      val newMust = must widening other.must
      // update state
      copy(
        may = newMay,
        must = newMust,
        expr = ExpressionSet(),
        pp = DummyProgramPoint
      )
    }
  }

  override def lessEqual(other: T): Boolean = {
    if (isBottom || other.isTop) true
    else if (isTop || other.isBottom) false
    else (may lessEqual other.may) && (must lessEqual other.must)
  }

  /* ------------------------------------------------------------------------- *
   * SILVER STATE METHODS
   */

  override def inhale(condition: Expression): T = {
    logger.trace(s"inhale($condition)")

    // inhale condition in may and must alias graph
    val (newMay, _) = may.inhale(condition)
    val (newMust, _) = must.inhale(condition)
    // return updated state
    copy(
      may = newMay,
      must = newMust,
      expr = ExpressionSet()
    )
  }

  override def exhale(condition: Expression): T = {
    logger.trace(s"exhale($condition)")

    // exhale condition in may and must alias graph
    val (newMay, _) = may.exhale(condition)
    val (newMust, _) = must.exhale(condition)
    // return updated state
    copy(
      may = newMay,
      must = newMust,
      expr = ExpressionSet()
    )
  }

  /* ------------------------------------------------------------------------- *
   * SIMPLE STATE METHODS
   */

  override def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    logger.trace(s"createVariable($variable)")

    // create variable in may and must alias graph
    val (newMay, _) = may.createVariable(variable)
    val (newMust, _) = must.createVariable(variable)
    // update state
    copy(may = newMay, must = newMust)
  }

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type): T =
    createVariable(variable, typ, DummyProgramPoint)

  override def assignVariable(target: Expression, value: Expression): T = {
    logger.trace(s"assignVariable($target, $value)")

    target match {
      case variable: VariableIdentifier =>
        // perform assignment in may and must alias graph
        val (newMay, _) = may.assignVariable(variable, value)
        val (newMust, _) = must.assignVariable(variable, value)
        // update state
        val updated = copy(
          may = newMay,
          must = newMust,
          expr = ExpressionSet()
        )
        // perform abstract garbage collection
        updated.pruneUnreachableHeap()
      case _ => ???
    }
  }

  override def assignField(target: Expression, field: String, value: Expression): T = {
    logger.trace(s"assignField($target, $value)")

    target match {
      case target: AccessPathIdentifier =>
        // perform assignment in may and must alias graph
        val (newMay, _) = may.assignField(target, value)
        val (newMust, _) = must.assignField(target, value)
        // update state
        val updated = copy(
          may = newMay,
          must = newMust,
          expr = ExpressionSet()
        )
        // perform abstract garbage collection
        updated.pruneUnreachableHeap()
      case _ => ???
    }
  }

  override def setVariableToTop(varExpr: Expression): T = ???

  override def removeVariable(varExpr: VariableIdentifier): T = ???

  override def getFieldValue(receiver: Expression, field: String, typ: Type): T = {
    logger.trace(s"getFieldValue($receiver, $field)")

    val identifier = VariableIdentifier(field)(typ)
    val access = receiver match {
      case variable: VariableIdentifier => AccessPathIdentifier(variable :: identifier :: Nil)
      case AccessPathIdentifier(path) => AccessPathIdentifier(path :+ identifier)
      case _ => ???
    }

    copy(expr = ExpressionSet(access))
  }

  override def assume(condition: Expression): T = inhale(condition)

  /* ------------------------------------------------------------------------- *
   * STATE METHODS
   */

  override def before(pp: ProgramPoint): T = copy(pp = pp)

  override def createObject(typ: Type, pp: ProgramPoint): T = {
    logger.trace(s"createObject($typ)")

    copy(expr = ExpressionSet(NewNode))
  }

  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    logger.trace(s"evalConstant($value)")

    val constant = Constant(value, typ)(pp)
    copy(expr = ExpressionSet(constant))
  }

  override def getVariableValue(identifier: Identifier): T = {
    logger.trace(s"getVariableValue($identifier)")

    identifier match {
      case variable: VariableIdentifier => copy(expr = ExpressionSet(variable))
      case _ => ???
    }
  }

  override def pruneUnreachableHeap(): T = {
    val (newMay, _) = may.garbageCollect()
    val (newMust, _) = must.garbageCollect()
    copy(may = newMay, must = newMust)
  }

  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = ???

  override def removeExpression(): T = copy(expr = ExpressionSet())

  override def setArgument(x: ExpressionSet, right: ExpressionSet): T = ???

  override def setExpression(expr: ExpressionSet): T = copy(expr = expr)

  override def throws(t: ExpressionSet): T = ???

  override def ids: IdentifierSet = ???

  /* ------------------------------------------------------------------------- *
   * ALIAS ANALYSIS STATE METHODS
   */

  /**
    * Returns whether the locations corresponding to the two given expressions
    * may alias.
    *
    * @param left  The expression corresponding to the first location.
    * @param right The expression corresponding to the second location.
    * @return True if the locations may alias.
    */
  def mayAlias(left: Expression, right: Expression): Boolean =
    may.mayAlias(left, right)

  /**
    * Returns whether the locations corresponding to the two given expressions
    * must alias.
    *
    * @param left  The expression corresponding to the first location.
    * @param right The expression corresponding to the second location.
    * @return True if the locations must alias.
    */
  def mustAlias(left: Expression, right: Expression): Boolean =
    must.mustAlias(left, right)

  /**
    * Copies the state and updates the top flag, bottom flag, may alias domain,
    * must alias domain, expression set, and program point if the corresponding
    * arguments are defined.
    *
    * @param isTop    The top flag.
    * @param isBottom The bottom flag.
    * @param may      The may alias domain.
    * @param must     The must alias domain.
    * @param expr     The expression set.
    * @param pp       The program point.
    * @return The updated state.
    */
  def copy(isTop: Boolean = isTop,
           isBottom: Boolean = isBottom,
           may: May = may,
           must: Must = must,
           expr: ExpressionSet = expr,
           pp: ProgramPoint = pp): T
}

/**
  * A state of the alias analysis.
  *
  * @param isTop    The top flag.
  * @param isBottom The bottom flag.
  * @param may      The may alias graph.
  * @param must     The must alias graph.
  * @param expr     The expression set.
  * @param pp       The current program point.
  * @author Jerome Dohrau
  */
case class SimpleAliasAnalysisState(isTop: Boolean,
                                    isBottom: Boolean,
                                    may: MayAliasGraph,
                                    must: MustAliasGraph,
                                    expr: ExpressionSet,
                                    pp: ProgramPoint)
  extends AliasAnalysisState[SimpleAliasAnalysisState, MayAliasGraph, MustAliasGraph]
    with LazyLogging {

  /**
    * The type of an alias analysis state.
    */
  type T = SimpleAliasAnalysisState

  /**
    * Copies the state and updates the top flag, bottom flag, may alias graph,
    * must alias graph, expression set, and program point if the corresponding
    * argument is defined.
    *
    * @param isTop    The top flag.
    * @param isBottom The bottom flag.
    * @param may      The may alias graph.
    * @param must     The must alias graph.
    * @param expr     The expression set.
    * @param pp       The program point.
    * @return The updated state.
    */
  override def copy(isTop: Boolean = isTop,
                    isBottom: Boolean = isBottom,
                    may: MayAliasGraph = may,
                    must: MustAliasGraph = must,
                    expr: ExpressionSet = expr,
                    pp: ProgramPoint = pp): T =
    SimpleAliasAnalysisState(isTop, isBottom, may, must, expr, pp)
}

/**
  * An entry state builder for the alias analysis.
  *
  * @author Jerome Dohrau
  */
case class AliasAnalysisEntryStateBuilder()
  extends SilverEntryStateBuilder[SimpleAliasAnalysisState] {

  override def default: SimpleAliasAnalysisState = SimpleAliasAnalysisState(
    isTop = false,
    isBottom = false,
    may = MayAliasGraph(),
    must = MustAliasGraph(),
    expr = ExpressionSet(),
    pp = DummyProgramPoint
  )

  override def build(program: SilverProgramDeclaration, method: SilverMethodDeclaration): SimpleAliasAnalysisState = {
    val fields = program.fields.map(_.variable.id)
    val initial = default.factory(fields)
    initializeArguments(initial, program, method)
  }
}

/**
  * The alias analysis.
  *
  * @author Jerome Dohrau
  */
case class AliasAnalysis()
  extends SilverForwardAnalysis[SimpleAliasAnalysisState] {

  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[SimpleAliasAnalysisState] = {
    // set context
    Context.setProgram(program)

    // build entry state
    val builder = AliasAnalysisEntryStateBuilder()
    val entry = builder.build(program, method)

    // run analysis
    analyze(method, entry)
  }
}

/**
  * A runner for the alias analysis.
  *
  * @author Jerome Dohrau
  */
object AliasAnalysis
  extends SilverAnalysisRunner[SimpleAliasAnalysisState] {

  override val analysis: SilverAnalysis[SimpleAliasAnalysisState] = AliasAnalysis()

  override def main(args: Array[String]): Unit = {
    assert(args.nonEmpty, "No file specified.")

    // run analysis
    val path = new File(args(0)).toPath
    val results = run(Compilable.Path(path))

    for (method <- results.identifiers) {
      println(method)
      results.getResult(method).print()
    }
  }
}