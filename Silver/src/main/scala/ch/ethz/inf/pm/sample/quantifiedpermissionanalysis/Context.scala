/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.execution.{CfgResult, TrackingCFGState}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, DefaultSilverConverter, PermType}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.NumericalAnalysisState.PolyhedraAnalysisState
import viper.silver.{ast => sil}

import scala.collection._

/**
  * @author Severin Münger
  *         Added on 29/10/16.
  */
object Context {

  type NumericalDomainType = Apron.Polyhedra

  type NumericalStateType = PolyhedraAnalysisState

  type NumericalStateBuilderType = PolyhedraAnalysisEntryState.type

  val numericalStateBuilder = PolyhedraAnalysisEntryState

//  type NumType = IntegerOctagons
//
//  type NumericalStateType = OctagonAnalysisState
//
//  type NumericalStateBuilderType = OctagonAnalysisEntryState.type
//
//  val numericalStateBuilder = OctagonAnalysisEntryState

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

  private var aliasesPerMethod: Map[String, Option[CfgResult[_ <: AliasAnalysisState[_]]]] = Map()

  private var numericalInfoPerMethod: Map[String, Option[CfgResult[NumericalStateType]]] = Map()

  /**
    * Stores the result of the alias analysis.
    */
  private var aliases: Option[CfgResult[_ <: AliasAnalysisState[_]]] = None

  /**
    * Stores the result of the numerical analysis.
    */
  private var numericalInfo: Option[CfgResult[NumericalStateType]] = None

  def getSetFor(key: (ProgramPoint, Expression)): sil.LocalVarDecl = {
    if (!sets.contains(key))
      sets.put(key, sil.LocalVarDecl(createNewUniqueSetIdentifier("set_" + extractSetName(key._2)), sil.SetType(DefaultSampleConverter.convert(key._2.typ)))())
    sets(key)
  }

  private def extractSetName(expr: Expression): String = expr match {
    case FieldExpression(_, field, rec) => extractSetName(rec) + "_" + field
    case VariableIdentifier(name, _) => name
    case FunctionCallExpression(functionName, _, _, _) => functionName
  }

  def setProgram(program: sil.Program): Unit = {
    this.program = program
    initContext
  }

  def functions(name: String): sil.FuncLike = {
    if (auxiliaryFunctions.contains(name)) auxiliaryFunctions(name) else programFunctions(name)
  }

  def clearMethodSpecificInfo(): Unit = {
    rdAmountVariable match {
      case Some(varDecl) =>
        identifiers -= varDecl.name
        rdAmountVariable = None
      case None =>
    }
    identifiers --= quantifiedVariables.values.flatten.map(varDecl => varDecl.name)
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
    identifiers ++= program.domains.flatMap(domain => domain.axioms.map(axiom => axiom.name) ++ domain.functions.map(function => function.name))
    programFunctions ++= program.functions.map(function => (function.name, function))
    programFunctions ++= program.domains.flatMap(domain => domain.functions.map(function => (function.name, function)))
  }

  private def createNewUniqueIdentifier(name: String, markAsTaken: Boolean = true) = {
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
    if (markAsTaken)
      identifiers += identifier
    identifier
  }

  def createNewUniqueVarIdentifier(name: String = "_var", markAsTaken: Boolean = true): String = createNewUniqueIdentifier(name, markAsTaken)

  def createNewUniqueFunctionIdentifier(name: String = "_func"): String = createNewUniqueIdentifier(name)

  private def createNewUniqueSetIdentifier(name: String = "_set"): String = createNewUniqueIdentifier(name)

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
    case Some(existingBoundaryFunction) => existingBoundaryFunction
    case None =>
      val fun = sil.Function(createNewUniqueFunctionIdentifier("bound"), Seq(VarXDecl), sil.Perm, Seq(), Seq(), Some(sil.CondExp(sil.PermLtCmp(VarX, ZeroPerm)(), ZeroPerm, VarX)()))()
      boundaryFunction = Some(fun)
      auxiliaryFunctions += ((fun.name, fun))
      fun
  }

  def getQuantifiedVarDecl(typ: sil.Type, exclude: Set[sil.LocalVarDecl] = Set()): sil.LocalVarDecl = {
    if ((quantifiedVariables.getOrElse(typ, Seq()).toSet -- exclude).isEmpty)
      quantifiedVariables += typ -> (quantifiedVariables.getOrElse(typ, Seq()) :+ sil.LocalVarDecl(createNewUniqueVarIdentifier("_" + getTypeName(typ)(0).toLower.toString), typ)())
    (quantifiedVariables.getOrElse(typ, Seq()).toSet -- exclude).head
  }

  def getQuantifiedVarDeclsForType(typ: sil.Type, number: Int, exclude: Set[sil.LocalVarDecl] = Set()): Seq[sil.LocalVarDecl] = {
    if (!quantifiedVariables.contains(typ))
      quantifiedVariables += typ -> Seq()
    for (_ <- 0 until Math.max(0, number - (quantifiedVariables(typ).toSet -- exclude).size))
      quantifiedVariables += typ -> (quantifiedVariables(typ) :+ sil.LocalVarDecl(createNewUniqueVarIdentifier("_" + getTypeName(typ)(0).toLower.toString), typ)())
    (quantifiedVariables(typ).toSet -- exclude).toSeq.take(number)
  }

  private def getTypeName(typ: sil.Type): String = DefaultSilverConverter.convert(typ).name

  /**
    * Sets the result of the alias analysis.
    *
    * @param aliases The result of the alias analysis to set.
    */
  def setAliases(method: String, aliases: Option[CfgResult[_ <: AliasAnalysisState[_]]]): Unit = {
    aliasesPerMethod += method -> aliases
  }

  def loadAliasesForMethod(method: String): Unit = aliases = aliasesPerMethod(method)

  /**
    * Returns the state of the alias analysis before the given program point.
    *
    * @param pp The program point.
    * @tparam A The type of the alias analysis.
    * @return The state of the alias analysis before the given program point.
    */
  def preAliases[A <: AliasAnalysisState[A]](pp: ProgramPoint): A =
    aliases.get.preStateAt(pp).asInstanceOf[A]

  /**
    * Returns the state of the alias analysis after the given program point.
    *
    * @param pp The program point.
    * @tparam A The type fo the alias analysis.
    * @return The state of the alias analysis after the given program point.
    */
  def postAliases[A <: AliasAnalysisState[A]](pp: ProgramPoint): A =
    aliases.get.postStateAt(pp).asInstanceOf[A]

  /**
    * Sets the result of the numerical analysis.
    *
    * @param numericalInfo The result of the numerical analysis to set.
    */
  def setNumericalInfo(method: String, numericalInfo: CfgResult[NumericalStateType]): Unit = {
    numericalInfoPerMethod += method -> Some(numericalInfo)
  }

  def loadNumericalInfoForMethod(method: String): Unit = numericalInfo = numericalInfoPerMethod(method)

  /**
    * Returns the state of the numerical analysis before the given program point.
    *
    * @param pp The program point.
    * @return The state of the numerical analysis before the given program point.
    */
  def preNumericalInfo(pp: ProgramPoint): NumericalStateType =
    numericalInfo.get.preStateAt(pp)

  /**
    * Returns the state of the alias analysis after the given program point.
    *
    * @param pp The program point.
    * @return The state of the alias analysis after the given program point.
    */
  def postNumericalInfo(pp: ProgramPoint): NumericalStateType =
    numericalInfo.get.postStateAt(pp)
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
