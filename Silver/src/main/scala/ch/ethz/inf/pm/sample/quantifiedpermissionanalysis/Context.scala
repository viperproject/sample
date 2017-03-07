/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.CfgResult
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionsParameters._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils.ExpressionBuilder._
import viper.silver.ast.Function
import viper.silver.{ast => sil}

import scala.collection._

/**
  * @author Severin Münger
  *         Added on 29/10/16.
  */
object Context {

  var program: sil.Program = _

  private var currentMethod: String = _

  private var identifiers: Set[String] = Set()

  private var boundaryFunction: Option[sil.Function] = None

  private var maxFunction: Option[sil.Function] = None

  private var rdAmountVariable: Option[sil.LocalVarDecl] = None

  private var quantifiedVariables: Map[sil.Type, Seq[sil.LocalVarDecl]] = Map()

  private var programFunctions: Map[String, sil.FuncLike] = Map()

  private var auxiliaryFunctions: Map[String, sil.Function] = Map()

  private var fieldAccessFunctions: Map[String, sil.Function] = Map()

  private var fieldAccessFunctionsInMethod: Map[String, Set[(String, sil.Function)]] = Map()

  private var sets: Map[(ProgramPoint, Expression), sil.LocalVarDecl] = Map()

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

  def getAuxiliaryFunctions: Map[String, Function] = auxiliaryFunctions

  def getFieldAccessFunction(field: String): sil.Function = {
    if (!fieldAccessFunctions.contains(field)) {
      val function = sil.Function(Context.createNewUniqueFunctionIdentifier("get_" + field), Seq(sil.LocalVarDecl("x", sil.Ref)()), program.findField(field).typ, Seq(), Seq(), None)()
      fieldAccessFunctions += field -> function
      auxiliaryFunctions += function.name -> function
    }
    fieldAccessFunctionsInMethod += currentMethod -> (fieldAccessFunctionsInMethod.getOrElse(currentMethod, Set()) + ((field, fieldAccessFunctions(field))))
    fieldAccessFunctions(field)
  }

  def getPlaceholderFunction(quantifiedVariable: sil.LocalVarDecl): sil.Function = {
    val function = sil.Function(Context.createNewUniqueFunctionIdentifier("p"), Seq(quantifiedVariable), sil.Perm, Seq(), Seq(), None)()
    auxiliaryFunctions += function.name -> function
    function
  }

  def getFieldAccessFunctionsForCurrentMethod: Set[(String, sil.Function)] = fieldAccessFunctionsInMethod.getOrElse(currentMethod, Set())

  def getSetFor(key: (ProgramPoint, Expression)): sil.LocalVarDecl = {
    if (!sets.contains(key))
      sets += key -> sil.LocalVarDecl(createNewUniqueSetIdentifier(extractSetName(key._2) match {
        case name if useShortHelperVariableNames => "s_" + name(0)
        case name => "set_" + name
      }), sil.SetType(DefaultSampleConverter.convert(key._2.typ)))()
    sets(key)
  }

  private def extractSetName(expr: Expression): String = expr match {
    case FieldExpression(_, field, rec) => extractSetName(rec) + "_" + field
    case VariableIdentifier(name, _) => name
    case FunctionCallExpression(functionName, _, _, _) => functionName
  }

  def setMethodContext(program: sil.Program, method: SilverMethodDeclaration): Unit = {
    this.program = program
    currentMethod = method.name.name
    // Add all existing identifiers to the identifiers set (fields, domain names, method names, function names etc.)
    identifiers ++= program.fields.map(_.name)
    identifiers ++= program.methods.flatMap(method => (method.formalArgs ++ method.formalReturns ++ method.locals).toSet).map(_.name)
    identifiers ++= program.methods.map(_.name)
    identifiers ++= program.domains.map(_.name)
    identifiers ++= program.functions.map(_.name)
    identifiers ++= program.predicates.map(_.name)
    identifiers ++= program.domains.flatMap(domain => domain.axioms.map(_.name) ++ domain.functions.map(_.name))
    programFunctions ++= program.functions.map(function => (function.name, function))
    programFunctions ++= program.domains.flatMap(domain => domain.functions.map(function => (function.name, function)))
  }

  def functions(name: String): sil.FuncLike = if (auxiliaryFunctions.contains(name)) auxiliaryFunctions(name) else programFunctions(name)

  def prepareMethodForExtension(name: String): Unit = {
    currentMethod = name
    loadAliasesForMethod(name)
    loadNumericalInfoForMethod(name)
    identifiers --= sets.values.map(_.name)
    sets = Map()
  }

  def clearMethodSpecificInfo(): Unit = {
    rdAmountVariable match {
      case Some(varDecl) =>
        identifiers -= varDecl.name
        rdAmountVariable = None
      case None =>
    }
    identifiers --= quantifiedVariables.values.flatten.map(_.name)
    quantifiedVariables = Map()
    sets = Map()
  }

  private def typeToVarPrefix(typ: sil.Type): Char = typ match {
    case sil.Int => 'q'
    case _ => DefaultSilverConverter.convert(typ).name.toLowerCase()(0)
  }

  private def createNewUniqueIdentifier(name: String, markAsTaken: Boolean = true): String = {
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

  def createNewUniqueVarIdentifier(name: Char): String = createNewUniqueIdentifier(name.toString)

  def createNewUniqueFunctionIdentifier(name: String = "_func"): String = createNewUniqueIdentifier(name)

  private def createNewUniqueSetIdentifier(name: String = "_set"): String = createNewUniqueIdentifier(name)

  def clearIdentifiers(identifiers: GenTraversableOnce[String]): Unit = this.identifiers --= identifiers

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
      quantifiedVariables += typ -> (quantifiedVariables.getOrElse(typ, Seq()) :+ sil.LocalVarDecl(createNewUniqueVarIdentifier(typeToVarPrefix(typ)), typ)())
    (quantifiedVariables.getOrElse(typ, Seq()).toSet -- exclude).head
  }

  def getQuantifiedVarDeclsForType(typ: sil.Type, number: Int, exclude: Set[sil.LocalVarDecl] = Set()): Seq[sil.LocalVarDecl] = {
    if (!quantifiedVariables.contains(typ))
      quantifiedVariables += typ -> Seq()
    for (_ <- 0 until Math.max(0, number - (quantifiedVariables(typ).toSet -- exclude).size))
      quantifiedVariables += typ -> (quantifiedVariables(typ) :+ sil.LocalVarDecl(createNewUniqueVarIdentifier(typeToVarPrefix(typ)), typ)())
    (quantifiedVariables(typ).toSet -- exclude).toSeq.take(number)
  }

  def getReplacements(variables: Set[Identifier]): Map[Identifier, Identifier] = variables.foldLeft[Map[Identifier, Identifier]](Map()) { case (existing, identifier) =>
    val replacement = VariableIdentifier(createNewUniqueVarIdentifier())(identifier.typ)
    existing + (identifier -> replacement)
  }

  /**
    * Sets the result of the alias analysis.
    *
    * @param aliases The result of the alias analysis to set.
    */
  def setAliases(method: String, aliases: Option[CfgResult[_ <: AliasAnalysisState[_]]]): Unit = {
    aliasesPerMethod += method -> aliases
    this.aliases = aliases
  }

  def loadAliasesForMethod(method: String): Unit = aliases = aliasesPerMethod(method)

  /**
    * Returns the state of the alias analysis before the given program point.
    *
    * @param pp The program point.
    * @tparam A The type of the alias analysis.
    * @return The state of the alias analysis before the given program point.
    */
  def preAliases[A <: AliasAnalysisState[A]](pp: ProgramPoint): A = aliases.get.preStateAt(pp).asInstanceOf[A]

  /**
    * Returns the state of the alias analysis after the given program point.
    *
    * @param pp The program point.
    * @tparam A The type fo the alias analysis.
    * @return The state of the alias analysis after the given program point.
    */
  def postAliases[A <: AliasAnalysisState[A]](pp: ProgramPoint): A = aliases.get.postStateAt(pp).asInstanceOf[A]

  /**
    * Sets the result of the numerical analysis.
    *
    * @param numericalInfo The result of the numerical analysis to set.
    */
  def setNumericalInfo(method: String, numericalInfo: CfgResult[NumericalStateType]): Unit = {
    numericalInfoPerMethod += method -> Some(numericalInfo)
    this.numericalInfo = Some(numericalInfo)
  }

  def loadNumericalInfoForMethod(method: String): Unit = numericalInfo = numericalInfoPerMethod(method)

  /**
    * Returns the state of the numerical analysis before the given program point.
    *
    * @param pp The program point.
    * @return The state of the numerical analysis before the given program point.
    */
  def preNumericalInfo(pp: ProgramPoint): NumericalStateType = numericalInfo.get.preStateAt(pp)

  /**
    * Returns the state of the alias analysis after the given program point.
    *
    * @param pp The program point.
    * @return The state of the alias analysis after the given program point.
    */
  def postNumericalInfo(pp: ProgramPoint): NumericalStateType = numericalInfo.get.postStateAt(pp)
}

object VarXDecl extends sil.LocalVarDecl("x", sil.Perm)()

object VarX extends sil.LocalVar("x")(sil.Perm)

object VarYDecl extends sil.LocalVarDecl("y", sil.Perm)()

object VarY extends sil.LocalVar("y")(sil.Perm)

object ZeroPerm extends sil.NoPerm()()

object WritePerm extends sil.FullPerm()()

trait ExpressionDescription extends Expression {
  def expr: Expression
  final def key: (ProgramPoint, Expression) = (pp, expr)
  def ids: IdentifierSet = expr.ids
  def contains(f: (Expression) => Boolean): Boolean = f(this) || expr.contains(f)
  override def find(f: (Expression) => Boolean): Option[Expression] = if (f(this)) Some(this) else expr.find(f)
  def transform(f: (Expression) => Expression): Expression = f(this)
}

object ExpressionDescription {
  def apply(pp: ProgramPoint, expr: Expression): ExpressionDescription = expr match {
    case FunctionCallExpression(functionName, parameters, typ, _) => FunctionExpressionDescription(functionName, parameters.map(parameter => parameter.typ match {
      case IntType => Right(IntegerSetDescription.Inner(pp, parameter))
      case _ => Left(ExpressionDescription(pp, parameter))
    } ), typ, pp)
    case _ => SimpleExpressionDescription(pp, expr)
  }
  def unapply(expressionDescription: ExpressionDescription): Option[(ProgramPoint, Expression)] = Some(expressionDescription.pp, expressionDescription.expr)
}

case class SimpleExpressionDescription(pp: ProgramPoint, expr: Expression) extends ExpressionDescription {
  def typ: Type = expr.typ
}

case class FunctionExpressionDescription(functionName: String, parameters: Seq[Either[ExpressionDescription, IntegerSetDescription]], typ: Type, pp: ProgramPoint) extends ExpressionDescription {
  def expr = FunctionCallExpression(functionName, parameters.map(_.fold(_.expr, _.expr)), typ, pp)
  def transformAssignVariable(left: VariableIdentifier): FunctionExpressionDescription = FunctionExpressionDescription(functionName, parameters.map {
    case Right(integerSetDescription) => Right(integerSetDescription.transformAssignVariable(left, null))
    case other => other
  }, typ, pp)
  def transformCondition(cond: Expression): FunctionExpressionDescription = FunctionExpressionDescription(functionName, parameters.map {
    case Right(integerSetDescription) => Right(integerSetDescription.transformCondition(cond))
    case other => other
  }, typ, pp)
  def getIntegerParams: Seq[IntegerSetDescription] = parameters.flatMap(_.fold(_ => Set[IntegerSetDescription](), Set(_)))
  def getNonIntegerParams: Seq[ExpressionDescription] = parameters.flatMap(_.fold(Set(_), _ => Set[ExpressionDescription]()))
}

case class MaxExpression(args: Seq[Expression], typ: Type, pp: ProgramPoint = DummyProgramPoint) extends Expression {
  def ids: IdentifierSet = args.map(_.ids).reduce(_ ++ _)
  def transform(f: (Expression) => Expression): Expression = f(MaxExpression(args.map(_.transform(f)), typ))
  def contains(f: (Expression) => Boolean): Boolean = f(this) || args.exists(_.contains(f))
  override def find(f: (Expression) => Boolean): Option[Expression] = if (f(this)) Some(this) else args.map(_.find(f)).reduce(_.orElse(_))
}

case class FractionalPermissionExpression(numerator: Int, denominator: Int) extends Expression {
  def pp: ProgramPoint = DummyProgramPoint
  def ids: IdentifierSet = IdentifierSet.Bottom
  def transform(f: (Expression) => Expression): Expression = f(this)
  def contains(f: (Expression) => Boolean): Boolean = f(this)
  override def find(f: (Expression) => Boolean): Option[Expression] = if (f(this)) Some(this) else None
  override def typ: Type = PermType
}

object PermissionExpression {
  def apply(fractional: FractionalPermission, reads: Int): Expression = (fractional, reads) match {
    case (ZeroPermission(), 0) => noneConst
    case (ZeroPermission(), _) => mult(intToConst(reads, PermType), VariableIdentifier(Context.getRdAmountVariable.name)(PermType))
    case (FractionalPermission(numerator, denominator), 0) => FractionalPermissionExpression(numerator, denominator)
    case (FractionalPermission(numerator, denominator), _) => plus(FractionalPermissionExpression(numerator, denominator), mult(intToConst(reads, PermType), VariableIdentifier(Context.getRdAmountVariable.name)(PermType)))
  }
  def unapply(expr: Expression): Option[(FractionalPermission, Int)] = expr match {
    case `noneConst` => Some(ZeroPermission(), 0)
    case BinaryArithmeticExpression(Constant(const, PermType, _), VariableIdentifier(name, _), ArithmeticOperator.*) if name == Context.getRdAmountVariable.name => Some(ZeroPermission(), const.toInt)
    case FractionalPermissionExpression(numerator, denominator) => Some(FractionalPermission(numerator, denominator), 0)
    case BinaryArithmeticExpression(FractionalPermissionExpression(numerator, denominator), BinaryArithmeticExpression(Constant(const, PermType, _), VariableIdentifier(name, _), ArithmeticOperator.*), ArithmeticOperator.+) if name == Context.getRdAmountVariable.name => Some(FractionalPermission(numerator, denominator), const.toInt)
    case _ => None
  }
}