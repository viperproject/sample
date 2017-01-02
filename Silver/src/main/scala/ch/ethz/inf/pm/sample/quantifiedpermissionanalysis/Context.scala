/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.execution.TrackingCFGState
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, PermType}
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState
import viper.silver.{ast => sil}

import scala.collection._

/**
  * @author Severin Münger
  *         Added on 29/10/16.
  */
object Context {

  var program: sil.Program = _

  val identifiers: mutable.Set[String] = mutable.Set()

  val auxiliaryFunctions: mutable.Map[String, sil.Function] = mutable.Map()

  val programFunctions: mutable.Map[String, sil.FuncLike] = mutable.Map()

  var maxFunction: Option[sil.Function] = None

  var boundaryFunction: Option[sil.Function] = None

  var quantifiedVariables: Map[sil.Type, Seq[sil.LocalVarDecl]] = Map()

  var rdAmountVariable: Option[sil.LocalVarDecl] = None

  val fieldAccessFunctions: mutable.Map[String, sil.Function] = mutable.Map()

  val sets: mutable.Map[(ProgramPoint, Expression), sil.LocalVarDecl] = mutable.Map()

  def getSetFor(key: (ProgramPoint, Expression)): sil.LocalVarDecl = {
    if (!sets.contains(key))
      sets.put(key, sil.LocalVarDecl(createNewUniqueSetIdentifier("set_" + extractSetName(key._2)), sil.SetType(DefaultSampleConverter.convert(key._2.typ)))())
    sets(key)
  }

  private def extractSetName(expr: Expression): String = expr match {
    case FieldExpression(_, field, rec) => extractSetName(rec) + "_" + field
    case VariableIdentifier(name, _) => name
    case FunctionCallExpression(_, functionName, _, _) => functionName
  }

  def setProgram(program: sil.Program): Unit = {
    this.program = program
    initContext
  }

  def functions(name: String): sil.FuncLike = {
    if (auxiliaryFunctions.contains(name)) auxiliaryFunctions(name) else programFunctions(name)
  }

  def clearMethodSpecificInfo(): Unit = {
    clearAliases()
    clearNumericalInfo()
    rdAmountVariable = None
    quantifiedVariables = Map()
    sets.clear()
  }

  private def initContext = {
    // Add all existing identifiers to the identifiers set (fields, domain names, method names, function names etc.)
    identifiers ++= program.fields.map(field => field.name)
    identifiers ++= program.methods.flatMap(method => (method.formalArgs ++ method.formalReturns ++ method.locals).toSet).map(varDecl => varDecl.name)
    identifiers ++= program.methods.map(method => method.name)
    identifiers ++= program.domains.map(domain => domain.name)
    identifiers ++= program.functions.map(function => function.name)
    identifiers ++= program.predicates.map(predicates => predicates.name)
    identifiers ++= program.domains.flatMap(domain => domain._axioms.map(axiom => axiom.name) ++ domain._functions.map(function => function.name))
    programFunctions ++= program.functions.map(function => (function.name, function))
    programFunctions ++= program.domains.flatMap(domain => domain._functions.map(function => (function.name, function)))
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
        Some(sil.CondExp(sil.PermLtCmp(VarX, ZeroPerm)(), ZeroPerm, VarX)())
      )()
      boundaryFunction = Some(fun)
      auxiliaryFunctions += ((fun.name, fun))
      fun
  }

  def getQuantifiedVarDecl(typ: sil.Type, exclude: Set[sil.LocalVarDecl] = Set()): sil.LocalVarDecl = {
    if ((quantifiedVariables.getOrElse(typ, Seq()).toSet -- exclude).isEmpty)
      quantifiedVariables += typ -> (quantifiedVariables.getOrElse(typ, Seq()) :+ sil.LocalVarDecl(createNewUniqueVarIdentifier("x"), typ)())
    (quantifiedVariables.getOrElse(typ, Seq()).toSet -- exclude).head
  }

  def getQuantifiedVarDeclsForType(typ: sil.Type, number: Int, exclude: Set[sil.LocalVarDecl] = Set()): Seq[sil.LocalVarDecl] = {
    if (!quantifiedVariables.contains(typ))
      quantifiedVariables += typ -> Seq()
    for (_ <- 0 to Math.max(0, number - (quantifiedVariables(typ).toSet -- exclude).size))
      quantifiedVariables(typ) :+= sil.LocalVarDecl(createNewUniqueVarIdentifier("x"), typ)()
    (quantifiedVariables(typ).toSet -- exclude).toSeq.take(number)
  }

  def getQuantifiedVarDecls(f: sil.FuncLike, exclude: Set[sil.LocalVarDecl]): Seq[sil.LocalVarDecl] = {
    val quantifiedVars = f.formalArgs.foldLeft[Map[sil.Type, Int]](Map())((map, varDecl) => map + (varDecl.typ -> (map.getOrElse(varDecl.typ, 0) + 1))).map {
      case (typ, count) => typ -> getQuantifiedVarDeclsForType(typ, count, exclude)
    }
    val counts: mutable.Map[sil.Type, Int] = mutable.Map()
    f.formalArgs.map {
      case sil.LocalVarDecl(_, typ) =>
        val index = counts.getOrElse(typ, 0)
        counts(typ) = index + 1
        quantifiedVars(typ)(index)
    }
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
  def preNumericalInfo[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]](pp: ProgramPoint): NumericalAnalysisState[N, T] =
    numericalInfo.get.preStateAt(position(pp)).asInstanceOf[T]

  /**
    * Returns the state of the alias analysis after the given program point.
    *
    * @param pp The program point.
    * @tparam T The type fo the numerical analysis.
    * @return The state of the alias analysis after the given program point.
    */
  def postNumericalInfo[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]](pp: ProgramPoint): NumericalAnalysisState[N, T] =
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
  override def typ: Type = PermType
  override def pp: ProgramPoint = DummyProgramPoint
  override def ids: IdentifierSet = IdentifierSet.Bottom
  override def transform(f: (Expression) => Expression): Expression = f(this)
  override def contains(f: (Expression) => Boolean): Boolean = f(this)
}

case class ExpressionDescription(pp: ProgramPoint, expr: Expression) extends Expression {
  override def typ: Type = expr.typ
  override def ids: IdentifierSet = expr.ids
  override def transform(f: (Expression) => Expression): ExpressionDescription = ExpressionDescription(pp, expr.transform(f))
  override def contains(f: (Expression) => Boolean): Boolean = f(this) || expr.contains(f)
  def key: (ProgramPoint, Expression) = (pp, expr)
}