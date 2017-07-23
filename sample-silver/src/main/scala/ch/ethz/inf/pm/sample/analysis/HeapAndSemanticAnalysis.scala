/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.analysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.IntegerOctagons
import ch.ethz.inf.pm.sample.domain.HeapNode.NewNode
import ch.ethz.inf.pm.sample.domain.{HeapAndSemanticDomain, HeapDomain, HeapNode, MayAliasGraph}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverAnalysisRunner, SilverMethodDeclaration, SilverProgramDeclaration}
import ch.ethz.inf.pm.sample.oorepresentation.{Compilable, DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.Expression
import com.typesafe.scalalogging.LazyLogging

/**
  * A state of an analysis that combines a heap domain with a semantic domain.
  *
  * This state essentially wraps an element of the heap and numerical domain
  * and adds all the state methods.
  *
  * @tparam T The type of the heap and semantic analysis state.
  * @tparam H The type of the elements of the heap domain.
  * @tparam S The type of the elements of the semantic domain.
  * @tparam I THe type of the identifiers used by the heap domain.
  * @author Jerome Dohrau
  */
trait HeapAndSemanticAnalysisState[T <: HeapAndSemanticAnalysisState[T, H, S, I], H <: HeapDomain[H, I], S <: SemanticDomain[S], I <: Identifier]
  extends SilverState[T]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {

  this: T =>

  type D = HeapAndSemanticDomain[H, S, I]

  /**
    * Returns the element of the heap and semantic domain.
    *
    * @return The element of the hap and semantic domain.
    */
  def domain: D

  /**
    * Returns the current program point.
    *
    * @return The current program point.
    */
  def pp: ProgramPoint

  /**
    * Creates an element of the heap and semantic analysis state with the given
    * fields.
    *
    * @param fields The fields.
    * @return The resulting state.
    */
  def factory(fields: Seq[Identifier]): T = copy(
    domain = domain.factory(fields),
    expr = ExpressionSet(),
    pp = DummyProgramPoint
  )

  /* ------------------------------------------------------------------------- *
   * LATTICE METHODS
   */

  override def factory(): T = copy(
    domain = domain.factory(),
    expr = ExpressionSet(),
    pp = DummyProgramPoint
  )

  override def top(): T = copy(
    domain = domain.top(),
    expr = ExpressionSet(),
    pp = DummyProgramPoint
  )

  override def bottom(): T = copy(
    domain = domain.bottom(),
    expr = ExpressionSet(),
    pp = DummyProgramPoint
  )

  override def lub(other: T): T = {
    logger.trace(s"lub($this, $other)")

    val newDomain = domain lub other.domain
    copy(domain = newDomain)
  }

  override def glb(other: T): T = {
    logger.trace(s"glb($this, other)")

    val newDomain = domain glb other.domain
    copy(domain = newDomain)
  }

  override def widening(other: T): T = {
    logger.trace(s"widening($this, other)")

    val newDomain = domain widening other.domain
    copy(domain = newDomain)
  }

  override def lessEqual(other: T): Boolean = {
    logger.trace(s"lessEqual($this, other)")

    domain lessEqual other.domain
  }

  override def isBottom: Boolean = domain.isBottom

  override def isTop: Boolean = domain.isTop

  /* ------------------------------------------------------------------------- *
   * SILVER STATE METHODS
   */

  override def inhale(expression: Expression): T = ???

  override def exhale(expression: Expression): T = ???

  /* ------------------------------------------------------------------------- *
   * SIMPLE STATE METHODS
   */

  override def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    logger.trace(s"createVariable($variable)")

    val newDomain = domain.createVariable(variable)
    copy(domain = newDomain)
  }

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type): T =
    createVariable(variable, typ, DummyProgramPoint)

  override def assignVariable(target: Expression, expression: Expression): T = {
    logger.trace(s"assignVariable($target, right)")

    target match {
      case variable: VariableIdentifier =>
        val newDomain = domain.assignVariable(variable, expression)
        copy(domain = newDomain)
      case _ => ???
    }
  }

  override def assignField(target: Expression, field: String, expression: Expression): T = {
    logger.trace(s"assignField($target, $expression)")

    target match {
      case target: AccessPathIdentifier =>
        val newDomain = domain.assignField(target, expression)
        copy(domain = newDomain)
      case _ => ???
    }
  }

  override def setVariableToTop(variable: Expression): T = ???

  override def removeVariable(variable: VariableIdentifier): T = {
    logger.trace(s"removeVariable($variable)")

    val newDomain = domain.removeVariable(variable)
    copy(domain = newDomain)
  }

  override def getFieldValue(receiver: Expression, field: String, typ: Type): T = {
    logger.trace(s"getFieldValue($receiver, $field)")

    val identifier = VariableIdentifier(field)(typ)
    val value = receiver match {
      case variable: VariableIdentifier => AccessPathIdentifier(variable :: identifier :: Nil)
      case AccessPathIdentifier(path) => AccessPathIdentifier(path :+ identifier)
      case _ => ???
    }

    copy(expr = ExpressionSet(value))
  }

  override def assume(condition: Expression): T = ???

  /* ------------------------------------------------------------------------- *
   * STATE METHODS
   */

  override def before(pp: ProgramPoint): T = copy(pp = pp)

  override def createObject(typ: Type, pp: ProgramPoint): T = {
    logger.trace(s"createObject($typ)")

    // TODO: This implementation is tailored for the use of the alias graph
    // and probably does not work in general.
    copy(expr = ExpressionSet(NewNode))
  }

  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    logger.trace(s"evalConstant($value)")

    val constant = Constant(value, typ, pp)
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
    val newDomain = domain.garbageCollect()
    copy(domain = newDomain)
  }

  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = ???

  override def removeExpression(): T = copy(expr = ExpressionSet())

  override def setArgument(x: ExpressionSet, right: ExpressionSet): T = ???

  override def setExpression(expr: ExpressionSet): T = copy(expr = expr)

  override def throws(t: ExpressionSet): T = ???

  override def ids: IdentifierSet = ???

  /* ------------------------------------------------------------------------- *
   * HEAP AND SEMANTIC ANALYSIS STATE METHODS
   */

  /**
    * Copies the state and updates the domain, expression set, and program point
    * if the corresponding arguments are defined.
    *
    * @param domain The heap and semantic domain.
    * @param expr   The expression set.
    * @param pp     The program point.
    * @return The updated state.
    */
  def copy(domain: D = domain,
           expr: ExpressionSet = expr,
           pp: ProgramPoint = pp): T
}

/**
  * A state of the heap and semantic analysis.
  *
  * @param domain The heap and semantic domain.
  * @param expr   The expression set.
  * @param pp     The current program point.
  * @tparam H The type of the elements of the heap domain.
  * @tparam S The type of the elements of the semantic domain.
  * @tparam I THe type of the identifiers used by the heap domain.
  * @author Jerome Dohrau
  */
case class SimpleHeapAndSemanticAnalysisState[H <: HeapDomain[H, I], S <: SemanticDomain[S], I <: Identifier](domain: HeapAndSemanticDomain[H, S, I],
                                                                                                              expr: ExpressionSet,
                                                                                                              pp: ProgramPoint)
  extends HeapAndSemanticAnalysisState[SimpleHeapAndSemanticAnalysisState[H, S, I], H, S, I] {

  /**
    * The type of an heap and semantic analysis state.
    */
  type T = SimpleHeapAndSemanticAnalysisState[H, S, I]

  override def copy(domain: D,
                    expr: ExpressionSet,
                    pp: ProgramPoint): T =
    SimpleHeapAndSemanticAnalysisState(domain, expr, pp)
}

/**
  * An entry state builder for a heap and semantic analysis.
  *
  * @tparam H The type of the heap domain.
  * @tparam S The type of the semantic domain.
  * @tparam I The type of the identifiers used by the heap domain.
  * @author Jerome Dohrau
  */
trait HeapAndSemanticAnalysisEntryStateBuilder[H <: HeapDomain[H, I], S <: SemanticDomain[S], I <: Identifier]
  extends SilverEntryStateBuilder[SimpleHeapAndSemanticAnalysisState[H, S, I]] {

  /**
    * Returns an element of the heap domain.
    *
    * @return An element of the heap domain.
    */
  def heap: H

  /**
    * Returns an element of the semantic domain.
    *
    * @return An element of the semantic domain.
    */
  def semantic: S

  override def default: SimpleHeapAndSemanticAnalysisState[H, S, I] = SimpleHeapAndSemanticAnalysisState(
    domain = HeapAndSemanticDomain(heap, semantic, Seq.empty),
    expr = ExpressionSet(),
    pp = DummyProgramPoint
  )

  override def build(program: SilverProgramDeclaration, method: SilverMethodDeclaration): SimpleHeapAndSemanticAnalysisState[H, S, I] = {
    val fields = program.fields.map(_.variable.id)
    val initial = default.factory(fields)
    initializeArguments(initial, program, method)
  }
}

/**
  * The entry state builder for the heap and octagon analysis.
  *
  * @author Jerome Dohrau
  */
object HeapAndOctagonAnalysisEntryState
  extends HeapAndSemanticAnalysisEntryStateBuilder[MayAliasGraph, IntegerOctagons, HeapNode] {
  override def heap: MayAliasGraph = MayAliasGraph()

  override def semantic: IntegerOctagons = IntegerOctagons.Top.factory()
}

/**
  * An analysis that combines a heap analysis with an integer octagon analysis.
  *
  * @author Jerome Dohrau
  */
object HeapAndOctagonAnalysis
  extends SilverAnalysisRunner[SimpleHeapAndSemanticAnalysisState[MayAliasGraph, IntegerOctagons, HeapNode]] {

  override val analysis: SilverAnalysis[SimpleHeapAndSemanticAnalysisState[MayAliasGraph, IntegerOctagons, HeapNode]] = SimpleSilverForwardAnalysis(HeapAndOctagonAnalysisEntryState)

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