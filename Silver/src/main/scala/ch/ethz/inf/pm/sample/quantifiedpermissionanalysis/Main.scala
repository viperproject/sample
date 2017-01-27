/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.abstractdomain.{Expression, FunctionCallExpression, VariableIdentifier}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState.SimpleAliasAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.{AliasAnalysisEntryState, AliasAnalysisStateBuilder}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionsParameters._
import ch.ethz.inf.pm.sample.{StdOutOutput, SystemParameters}
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 19/10/16.
  */
object Main {
  def main(args: Array[String]): Unit = {
    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()
    SystemParameters.wideningLimit = 10
    QuantifiedPermissionsAnalysisRunner.main(args)
  }
}

/** SIL analysis runner that uses the default QuantifiedPermissions analysis. */
object QuantifiedPermissionsAnalysisRunner extends SilverInferenceRunner[Any, QuantifiedPermissionsState] {
  SystemParameters.isValueDrivenHeapAnalysis = false

  override def extendProgram(program: sil.Program, cfgResults: Map[SilverIdentifier, CfgResult[QuantifiedPermissionsState]]): sil.Program = {
    val tempProg = super.extendProgram(program, cfgResults)
    tempProg.copy(functions = tempProg.functions ++ Context.auxiliaryFunctions.values)(pos = tempProg.pos, info = tempProg.info)
  }

  override def extendMethod(method: sil.Method, cfgResult: CfgResult[QuantifiedPermissionsState]): sil.Method = {
    Context.loadAliasesForMethod(method.name)
    Context.loadNumericalInfoForMethod(method.name)
    super.extendMethod(method, cfgResult)
  }

  val analysis = ForwardAndBackwardAnalysis(AliasAnalysisEntryState)

  private def toSilPerm(perm: FractionalPermission): sil.PermExp = perm match {
    case FractionalPermission(1, 1) => sil.FullPerm()()
    case FractionalPermission(0, _) => sil.NoPerm()()
    case FractionalPermission(numerator, denominator) => sil.FractionalPerm(sil.IntLit(numerator)(), sil.IntLit(denominator)())()
  }

  private def getMaxRdValue(permAmount: FractionalPermission, readAmount: Int): FractionalPermission = permAmount match {
    case FractionalPermission(numerator, denominator) =>
      if (readAmount == 0) FractionalPermission(1, 1)
      else FractionalPermission(denominator - numerator, denominator * readAmount)
  }

  private def getMaxRdValueTupled = (getMaxRdValue _).tupled

  /**
    * Modifies the list of preconditions using the specifications provided by
    * the given state.
    *
    * @param existing The list of existing preconditions.
    * @param state    The state providing the specifications.
    * @return The modified list of preconditions.
    */
  override def preconditions(existing: Seq[sil.Exp], state: QuantifiedPermissionsState): Seq[sil.Exp] = {
    var newPreconditions = existing
    if (state.permissions.exists((arg) => arg._2.exists {
      case PermissionLeaf(_, SymbolicReadPermission) => true
      case _ => false
    })) {
      val rdAmount = Context.getRdAmountVariable.localVar
      val readPaths = state.permissions.flatMap { case (_, tree) => tree.getReadAmounts }.toSet
      if (readPaths.nonEmpty) {
        var min = getMaxRdValue(readPaths.head._1, readPaths.head._2)
        readPaths.foreach { cur => if (getMaxRdValueTupled(cur) < min) min = getMaxRdValueTupled(cur) }
        newPreconditions :+= sil.And(sil.PermLtCmp(ZeroPerm, rdAmount)(), sil.PermLtCmp(rdAmount, toSilPerm(min))())()
      } else {
        newPreconditions :+= sil.And(sil.PermLtCmp(ZeroPerm, rdAmount)(), sil.PermLtCmp(rdAmount, WritePerm)())()
      }
    }
    state.permissions.foreach { case (fieldName, permissionTree) =>
      if (permissionTree.canBeExpressedByIntegerQuantification(state.refSets) && {
        val concreteExpressions = permissionTree.getSetDescriptions(state.refSets).flatMap(set => set.concreteExpressions)
        val elem: FunctionCallExpression = concreteExpressions.head._1.asInstanceOf[FunctionCallExpression]
        concreteExpressions.forall {
          case (FunctionCallExpression(functionName, parameters, _, _), _) =>
            functionName == elem.functionName && parameters.zip(elem.parameters).forall {
              case (left, right) => left.typ == IntType || left.equals(right)
            }
          case _ => throw new IllegalStateException()
        }
      }) {
        val quantifiedVariableDecl = Context.getQuantifiedVarDecl(sil.Int)
        val quantifiedVariable = quantifiedVariableDecl.localVar
        val fieldAccessReceiver = permissionTree.getSetDescriptions(state.refSets).head.concreteExpressions.head._1.transform {
          case e: Expression if e.typ == IntType => VariableIdentifier(quantifiedVariable.name)(IntType)
          case other => other
        }
        val fieldAccess = viper.silver.ast.FieldAccess(DefaultSampleConverter.convert(fieldAccessReceiver), Context.program.findField(fieldName))()
        val implies = sil.FieldAccessPredicate(fieldAccess, permissionTree.toIntegerQuantification(state, quantifiedVariable))()
        val forall = sil.Forall(Seq(quantifiedVariableDecl), Seq(), implies)()
        newPreconditions :+= forall
      } else {
        val quantifiedVariableDecl = Context.getQuantifiedVarDecl(sil.Ref)
        val quantifiedVariable = quantifiedVariableDecl.localVar
        val fieldAccess = viper.silver.ast.FieldAccess(quantifiedVariable, Context.program.findField(fieldName))()
        val implies = sil.FieldAccessPredicate(fieldAccess, permissionTree.toSilExpression(state, quantifiedVariable))()
        val forall = sil.Forall(Seq(quantifiedVariableDecl), Seq(), implies)()
        newPreconditions :+= forall
      }
    }
    Context.fieldAccessFunctions.foreach {
      case (fieldName, function) =>
        val quantifiedVarDecl = Context.getQuantifiedVarDecl(sil.Ref)
        val quantifiedVar = quantifiedVarDecl.localVar
        val field = sil.Field(fieldName, function.typ)()
        val implies = sil.Implies(sil.PermGtCmp(sil.CurrentPerm(sil.FieldAccess(quantifiedVar, field)())(), ZeroPerm)(), sil.EqCmp(sil.FuncApp(function, Seq(quantifiedVar))(), sil.FieldAccess(quantifiedVar, field)())())()
        newPreconditions :+= sil.InhaleExhaleExp(sil.Forall(Seq(quantifiedVarDecl), Seq(), implies)(), sil.TrueLit()())()
    }
    state.refSets.values.toSet.foreach((set: ReferenceSetDescription) => set match {
      case setDescription: ReferenceSetDescription.Inner =>
        if (!setDescription.isFinite(state.refSets) && !setDescription.canBeExpressedByIntegerQuantification(state.refSets))
          newPreconditions ++= setDescription.toSetDefinition(state)
      case _ => throw new IllegalStateException()
    })
    newPreconditions
  }

  /**
    * Modifies the list of postconditions using the specifications provided by
    * the given state.
    *
    * @param existing The list of existing postconditions.
    * @param state    The state providing the specifications.
    * @return The modified list of postconditions.
    */
  override def postconditions(existing: Seq[sil.Exp], state: QuantifiedPermissionsState): Seq[sil.Exp] = existing

  /**
    * Modifies the list of invariants using the specifications provided by
    * the given state.
    *
    * @param existing The list of existing invariants.
    * @param state    The state providing the specifications.
    * @return The modified list of invariants.
    */
  override def invariants(existing: Seq[sil.Exp], state: QuantifiedPermissionsState): Seq[sil.Exp] = {
    var newInvariants = existing
    state.permissions.foreach { case (fieldName, permissionTree) =>
      if (permissionTree.canBeExpressedByIntegerQuantification(state.refSets) && {
        val concreteExpressions = permissionTree.getSetDescriptions(state.refSets).flatMap(set => set.concreteExpressions)
        val elem: FunctionCallExpression = concreteExpressions.head._1.asInstanceOf[FunctionCallExpression]
        concreteExpressions.forall {
          case (FunctionCallExpression(functionName, parameters, _, _), _) =>
            functionName == elem.functionName && parameters.zip(elem.parameters).forall {
              case (left, right) => left.typ == IntType || left.equals(right)
            }
          case _ => throw new IllegalStateException()
        }
      }) {
        val quantifiedVariableDecl = Context.getQuantifiedVarDecl(sil.Int)
        val quantifiedVariable = quantifiedVariableDecl.localVar
        val fieldAccessReceiver = permissionTree.getSetDescriptions(state.refSets).head.concreteExpressions.head._1.transform {
          case e: Expression if e.typ == IntType => VariableIdentifier(quantifiedVariable.name)(IntType)
          case other => other
        }
        val fieldAccess = viper.silver.ast.FieldAccess(DefaultSampleConverter.convert(fieldAccessReceiver), Context.program.findField(fieldName))()
        val implies = sil.FieldAccessPredicate(fieldAccess, permissionTree.toIntegerQuantification(state, quantifiedVariable))()
        val forall = sil.Forall(Seq(quantifiedVariableDecl), Seq(), implies)()
        newInvariants :+= forall
      } else {
        val quantifiedVariableDecl = Context.getQuantifiedVarDecl(sil.Ref)
        val quantifiedVariable = quantifiedVariableDecl.localVar
        val fieldAccess = viper.silver.ast.FieldAccess(quantifiedVariable, Context.program.findField(fieldName))()
        val implies = sil.FieldAccessPredicate(fieldAccess, permissionTree.toSilExpression(state, quantifiedVariable))()
        val forall = sil.Forall(Seq(quantifiedVariableDecl), Seq(), implies)()
        newInvariants :+= forall
      }
    }
    Context.fieldAccessFunctions.foreach {
      case (fieldName, function) =>
        val quantifiedVarDecl = Context.getQuantifiedVarDecl(sil.Ref)
        val quantifiedVar = quantifiedVarDecl.localVar
        val field = sil.Field(fieldName, function.typ)()
        val implies = sil.Implies(sil.PermGtCmp(sil.CurrentPerm(sil.FieldAccess(quantifiedVar, field)())(), ZeroPerm)(), sil.EqCmp(sil.FuncApp(function, Seq(quantifiedVar))(), sil.FieldAccess(quantifiedVar, field)())())()
        newInvariants :+= sil.InhaleExhaleExp(sil.Forall(Seq(quantifiedVarDecl), Seq(), implies)(), sil.TrueLit()())()
    }
    state.refSets.values.toSet.foreach((set: ReferenceSetDescription) => set match {
      case setDescription: ReferenceSetDescription.Inner =>
        if (!setDescription.isFinite(state.refSets) && !setDescription.canBeExpressedByIntegerQuantification(state.refSets))
          newInvariants ++= setDescription.toSetDefinition(state)
      case _ => throw new IllegalStateException()
    })
    val numDom: NumericalDomain[_] = Context.postNumericalInfo(state.currentPP).numDom.removeVariables(state.declaredBelowVars)
    val constraints = numDom.getConstraints(numDom.ids.getNonTop)
    if (constraints.nonEmpty) newInvariants :+= numDom.getConstraints(numDom.ids.getNonTop).map(expr => DefaultSampleConverter.convert(expr)).reduce((left, right) => sil.And(left, right)())
    newInvariants
  }

  /**
    * Modifies the list of fields of a new statement using specifications
    * provided by the given field.
    *
    * @param existing The existing list of fields.
    * @param state    The state providing the specifications.
    * @return The modified list of fields.
    */
  override def fields(existing: Seq[sil.Field], state: QuantifiedPermissionsState): Seq[sil.Field] = existing

  /**
    * Modifies the list of formal arguments using information stored in the
    * current state.
    *
    * @param existing The list of existing formal arguments.
    * @return The modified list of formal arguments
    */
  override def formalArguments(existing: Seq[sil.LocalVarDecl], state: QuantifiedPermissionsState): Seq[sil.LocalVarDecl] = {
    var newFormalArguments = existing
    if (state.permissions.exists(_._2.exists {
      case PermissionLeaf(_, SymbolicReadPermission) => true
      case _ => false
    })) {
      val rdAmount = Context.getRdAmountVariable
      if (!newFormalArguments.contains(rdAmount)) newFormalArguments :+= rdAmount
    }
    state.refSets.foreach {
      case (_, setDescription: ReferenceSetDescription.Inner) =>
        if (!setDescription.isFinite(state.refSets) && !newFormalArguments.contains(Context.getSetFor(setDescription.key)))
          newFormalArguments :+= Context.getSetFor(setDescription.key)
      case _ => throw new IllegalStateException()
    }
    newFormalArguments
  }
}

case class ForwardAndBackwardAnalysis(aliasAnalysisBuilder: AliasAnalysisStateBuilder[SimpleAliasAnalysisState])
  extends SilverAnalysis[QuantifiedPermissionsState] with LazyLogging {

  def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[QuantifiedPermissionsState] = {

    Context.clearMethodSpecificInfo()

    Context.setProgram(DefaultSilverConverter.prog)

    // TODO: just catching the exception is a hack, better perform a search through the program if there exists a field access with a function as receiver
//    val aliasAnalysisResult: Option[CfgResult[SimpleAliasAnalysisState]] =
//      try {
//        val aliasEntry = aliasAnalysisBuilder.build(program, method)
//        val aliasInterpreter = FinalResultForwardInterpreter[SimpleAliasAnalysisState]()
//        val aliasResult = aliasInterpreter.execute(method.body, aliasEntry)
//        Some(aliasResult)
//      } catch {
//        case _: IllegalArgumentException =>
//          println("Warning: Heap analysis failed!")
//          None
//      }
//
    Context.setAliases(method.name.name.toString, None)//aliasAnalysisResult)

    val numericalEntry = numericalStateBuilder.build(program, method)
    val numericalInterpreter = FinalResultForwardInterpreter[NumericalStateType]()
    val numericalResult = numericalInterpreter.execute(method.body, numericalEntry)

    Context.setNumericalInfo(method.name.name.toString, numericalResult)

    val quantifiedPermissionsEntry = QuantifiedPermissionsState()
    val quantifiedPermissionsInterpreter = new QPInterpreter
    val quantifiedPermissionsResult = quantifiedPermissionsInterpreter.execute(method.body, quantifiedPermissionsEntry)

    quantifiedPermissionsResult
  }
}