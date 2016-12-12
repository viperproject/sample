/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, IdentifierSet}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.execution.TrackingCFGState
import ch.ethz.inf.pm.sample.oorepresentation.silver.{PermType, RefType}
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState
import viper.silver.{ast => sil}

import scala.collection._

/**
  * Created by severin on 29.10.16.
  */
object Context {

  var prog: sil.Program = _

  val identifiers: mutable.Set[String] = mutable.Set()

  val auxiliaryFunctions: mutable.Map[String, sil.Function] = mutable.Map()

  val programFunctions: mutable.Map[String, sil.FuncLike] = mutable.Map()

  var maxFunction: Option[sil.Function] = None

  var boundaryFunction: Option[sil.Function] = None

  var quantifiedVariable: Option[sil.LocalVarDecl] = None

  var rdAmountVariable: Option[sil.LocalVarDecl] = None

  val fieldAccessFunctions: mutable.Map[String, sil.Function] = mutable.Map()

  def setProgram(prog: sil.Program): Unit = {
    this.prog = prog
    initContext
  }

  def functions(name: String): sil.FuncLike = {
    if (auxiliaryFunctions.contains(name)) auxiliaryFunctions(name) else programFunctions(name)
  }

  def clearMethodSpecificInfo(): Unit = {
    clearAliases()
    clearNumericalInfo()
    rdAmountVariable = None
    quantifiedVariable = None
  }

  private def initContext = {
    // Add all existing identifiers to the identifiers set (fields, domain names, method names, function names etc.)
    identifiers ++= prog.fields.map(field => field.name)
    identifiers ++= prog.methods.flatMap(method => (method.formalArgs ++ method.formalReturns ++ method.locals).toSet).map(varDecl => varDecl.name)
    identifiers ++= prog.methods.map(method => method.name)
    identifiers ++= prog.domains.map(domain => domain.name)
    identifiers ++= prog.functions.map(function => function.name)
    identifiers ++= prog.predicates.map(predicates => predicates.name)
    identifiers ++= prog.domains.flatMap(domain => domain._axioms.map(axiom => axiom.name) ++ domain._functions.map(function => function.name))
    programFunctions ++= prog.functions.map(function => (function.name, function))
    programFunctions ++= prog.domains.flatMap(domain => domain._functions.map(function => (function.name, function)))
  }

  private def createNewUniqueIdentifier(name: String) = {
    var identifier: String = ""
    if (identifiers.contains(name)) {
      var count = 0
      while (identifiers.contains(name + count)) {
        count += 1
      }
      identifier = name + count
    } else {
      identifier = name
    }
    identifiers += identifier
    identifier
  }

  def createNewUniqueVarIdentifier(name: String = "_var"): String = createNewUniqueIdentifier(name)

  def createNewUniqueFunctionIdentifier(name: String = "_func"): String = createNewUniqueIdentifier(name)

  def createNewUniqueSetIdentifier(name: String = "_set"): String = createNewUniqueIdentifier(name)

  def getRdAmountVariable: sil.LocalVarDecl = rdAmountVariable match {
    case Some(existingRdAmountVar) => existingRdAmountVar
    case None =>
      val varDecl = sil.LocalVarDecl(createNewUniqueVarIdentifier("rdAmount"), sil.Perm)()
      rdAmountVariable = Some(varDecl)
      varDecl
  }

  def getMaxFunction: sil.Function = maxFunction match {
    case Some(existingMaxFunction) => existingMaxFunction
    case None =>
      val fun = sil.Function(createNewUniqueFunctionIdentifier("max"), Seq(VarXDecl, VarYDecl), sil.Perm, Seq(), Seq(),
        Some(sil.CondExp(sil.PermGtCmp(VarX, VarY)(), VarX, VarY)())
      )()
      maxFunction = Some(fun)
      auxiliaryFunctions += ((fun.name, fun))
      fun
  }

  def getBoundaryFunction: sil.Function = boundaryFunction match {
    case Some(existingMaxFunction) => existingMaxFunction
    case None =>
      val fun = sil.Function(createNewUniqueFunctionIdentifier("bound"), Seq(VarXDecl), sil.Perm, Seq(), Seq(),
        Some(sil.CondExp(sil.PermLtCmp(VarX, ZeroPerm)(), VarX, ZeroPerm)())
      )()
      boundaryFunction = Some(fun)
      auxiliaryFunctions += ((fun.name, fun))
      fun
  }

  def getQuantifiedVarDecl: sil.LocalVarDecl = quantifiedVariable match {
    case Some(existingQuantifiedVarDecl) => existingQuantifiedVarDecl
    case None =>
      val varDecl = sil.LocalVarDecl(createNewUniqueVarIdentifier("x"), sil.Ref)()
      quantifiedVariable = Some(varDecl)
      varDecl
  }

  /**
    * Stores the result of the alias analysis.
    */
  private var aliases: Option[TrackingCFGState[_ <: AliasAnalysisState[_]]] = None

  /**
    * Stores the result of the numerical analysis.
    */
  private var numericalInfo: Option[TrackingCFGState[_ <: NumericalAnalysisState[_ <: NumericalDomain[_], _]]] = None

  /**
    * Sets the result of the alias analysis.
    *
    * @param aliases The result of the alias analysis to set.
    * @tparam A The type of the alias analysis.
    */
  def setAliases[A <: AliasAnalysisState[A]](aliases: TrackingCFGState[A]): Unit = {
    this.aliases = Some(aliases)
  }

  /**
    * Clears the result of the alias analysis.
    */
  def clearAliases(): Unit = {
    this.aliases = None
  }

  /**
    * Returns the state of the alias analysis before the given program point.
    *
    * @param pp The program point.
    * @tparam A The type of the alias analysis.
    * @return The state of the alias analysis before the given program point.
    */
  def preAliases[A <: AliasAnalysisState[A]](pp: ProgramPoint): A =
  aliases.get.preStateAt(position(pp)).asInstanceOf[A]

  /**
    * Returns the state of the alias analysis after the given program point.
    *
    * @param pp The program point.
    * @tparam A The type fo the alias analysis.
    * @return The state of the alias analysis after the given program point.
    */
  def postAliases[A <: AliasAnalysisState[A]](pp: ProgramPoint): A =
  aliases.get.postStateAt(position(pp)).asInstanceOf[A]

  /**
    * Sets the result of the numerical analysis.
    *
    * @param numericalInfo The result of the numerical analysis to set.
    * @tparam T The type of the numerical analysis.
    */
  def setNumericalInfo[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]](numericalInfo: TrackingCFGState[T]): Unit = {
    this.numericalInfo = Some(numericalInfo)
  }

  /**
    * Clears the result of the numerical analysis.
    */
  def clearNumericalInfo(): Unit = {
    this.numericalInfo = None
  }

  /**
    * Returns the state of the numerical analysis before the given program point.
    *
    * @param pp The program point.
    * @tparam T The type of the numerical analysis.
    * @return The state of the numerical analysis before the given program point.
    */
  def preNumericalInfo[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]](pp: ProgramPoint): T =
    numericalInfo.get.preStateAt(position(pp)).asInstanceOf[T]

  /**
    * Returns the state of the alias analysis after the given program point.
    *
    * @param pp The program point.
    * @tparam T The type fo the numerical analysis.
    * @return The state of the alias analysis after the given program point.
    */
  def postNumericalInfo[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]](pp: ProgramPoint): T =
    numericalInfo.get.postStateAt(position(pp)).asInstanceOf[T]

  /**
    * Returns the cfg position corresponding to the given program point.
    *
    * @param pp The program point.
    * @return The cfg position corresponding to the given program point.
    */
  private def position(pp: ProgramPoint): CFGPosition = {
    val cfg = numericalInfo.get.cfg
    val positions = for {
      (block, i) <- cfg.nodes.zipWithIndex
      (statement, j) <- block.zipWithIndex
      if statement.getPC() == pp
    } yield CFGPosition(i, j)
    if (positions.nonEmpty) positions.head else null
  }
}

object VarXDecl extends sil.LocalVarDecl("x", sil.Perm)()

object VarX extends sil.LocalVar("x")(sil.Perm)

object VarYDecl extends sil.LocalVarDecl("y", sil.Perm)()

object VarY extends sil.LocalVar("y")(sil.Perm)

object ZeroPerm extends sil.NoPerm()()

object WritePerm extends sil.FullPerm()()

object ReadPermission extends Expression {
  /** The type of this expression. */
  override def typ: Type = PermType

  /** Point in the program where this expression is located. */
  override def pp: ProgramPoint = DummyProgramPoint

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = IdentifierSet.Bottom

  /** Runs f on the expression and all sub-expressions
    *
    * This also replaces identifiers inside heap ID sets.
    *
    * @param f the transformer
    * @return the transformed expression*/
  override def transform(f: (Expression) => Expression): Expression = f(this)

  /** Checks if function f evaluates to true for any sub-expression. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this)
}

case class ExpressionDescription(pp: ProgramPoint, expr: Expression) extends Expression {
  /** The type of this expression. */
  override def typ: Type = RefType()

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = expr.ids

  /** Runs f on the expression and all sub-expressions
    *
    * This also replaces identifiers inside heap ID sets.
    *
    * @param f the transformer
    * @return the transformed expression*/
  override def transform(f: (Expression) => Expression): ExpressionDescription = ExpressionDescription(pp, expr.transform(f))

  /** Checks if function f evaluates to true for any sub-expression. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this) || expr.contains(f)
}