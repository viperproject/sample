/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.abstractdomain.Expression
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
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
    tempProg.copy(functions = tempProg.functions ++ Context.getAuxiliaryFunctions.values)(pos = tempProg.pos, info = tempProg.info)
  }

  override def extendMethod(method: sil.Method, cfgResult: CfgResult[QuantifiedPermissionsState]): sil.Method = {
    Context.prepareMethodForExtension(method.name)
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
      else FractionalPermission(Math.max(denominator - numerator, 1), denominator * readAmount)
  }

  private def getMaxRdValueTupled = (getMaxRdValue _).tupled

  private def generateSpecifications(state: QuantifiedPermissionsState): Seq[sil.Exp] = {
    var specs = Seq[sil.Exp]()
    state.permissions.foreach { case (fieldName, permissionTree) =>
      if (permissionTree.isIntegerDependent) {
        val quantifiedVariableDecl = Context.getQuantifiedVarDecl(sil.Int)
        specs ++= permissionTree.transformForgetVariables(state.declaredBelowVars).toForgottenTree.toSilAssertions(quantifiedVariableDecl, Context.program.findField(fieldName))
      } else {
        val quantifiedVariableDecl = Context.getQuantifiedVarDecl(sil.Ref)
        val quantifiedVariable = quantifiedVariableDecl.localVar
        val fieldAccess = viper.silver.ast.FieldAccess(quantifiedVariable, Context.program.findField(fieldName))()
        val implies = sil.FieldAccessPredicate(fieldAccess, permissionTree.toSilExpression(state, quantifiedVariable))()
        val forall = sil.Forall(Seq(quantifiedVariableDecl), Seq(), implies)()
        specs :+= forall
      }
    }
    Context.getFieldAccessFunctionsForCurrentMethod.foreach { case (fieldName, function) =>
      val quantifiedVarDecl = Context.getQuantifiedVarDecl(sil.Ref)
      val quantifiedVar = quantifiedVarDecl.localVar
      val field = sil.Field(fieldName, function.typ)()
      val implies = sil.Implies(sil.PermGtCmp(sil.CurrentPerm(sil.FieldAccess(quantifiedVar, field)())(), ZeroPerm)(), sil.EqCmp(sil.FuncApp(function, Seq(quantifiedVar))(), sil.FieldAccess(quantifiedVar, field)())())()
      specs :+= sil.InhaleExhaleExp(sil.Forall(Seq(quantifiedVarDecl), Seq(), implies)(), sil.TrueLit()())()
    }
    var visited: Set[(ProgramPoint, Expression)] = Set()
    state.refSets.values.toSet.foreach((set: ReferenceSetDescription) => set match {
      case setDescription: ReferenceSetDescription.Inner =>
        if (!setDescription.isFinite(state) && !visited.contains(setDescription.key)) {
          specs ++= setDescription.toSetDefinition(state)
          visited += setDescription.key
        }
      case _ => throw new IllegalStateException()
    })
    specs
  }

  /**
    * Modifies the list of preconditions using the specifications provided by
    * the given state.
    *
    * @param existing The list of existing preconditions.
    * @param state    The state providing the specifications.
    * @return The modified list of preconditions.
    */
  override def preconditions(existing: Seq[sil.Exp], state: QuantifiedPermissionsState): Seq[sil.Exp] = {
    var preconditions = existing
    if (state.permissions.exists((arg) => arg._2.hasRead)) {
      val rdAmount = Context.getRdAmountVariable.localVar
      val readPaths = state.permissions.flatMap { case (_, tree) => tree.getReadAmounts }.toSet
      if (readPaths.nonEmpty) {
        var min = getMaxRdValue(readPaths.head._1, readPaths.head._2)
        readPaths.foreach { cur => if (getMaxRdValueTupled(cur) < min) min = getMaxRdValueTupled(cur) }
        preconditions :+= sil.And(sil.PermLtCmp(ZeroPerm, rdAmount)(), sil.PermLtCmp(rdAmount, toSilPerm(min))())()
      } else {
        preconditions :+= sil.And(sil.PermLtCmp(ZeroPerm, rdAmount)(), sil.PermLtCmp(rdAmount, WritePerm)())()
      }
    }
    preconditions ++= generateSpecifications(state)
    preconditions
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
    var newInvariants = existing ++ generateSpecifications(state)
    val numDom: NumericalDomain[_] = Context.preNumericalInfo(state.currentPP).numDom.removeVariables(state.declaredBelowVars)
    val constraints = numDom.getConstraints(numDom.ids.getNonTop)
    if (!numDom.isBottom && constraints.nonEmpty) newInvariants :+= constraints.map(DefaultSampleConverter.convert).reduce(sil.And(_, _)())
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
    if (state.permissions.exists(_._2.hasRead)) {
      val rdAmount = Context.getRdAmountVariable
      if (!newFormalArguments.contains(rdAmount)) newFormalArguments :+= rdAmount
    }
    state.refSets.foreach {
      case (_, setDescription: ReferenceSetDescription.Inner) =>
        if (!setDescription.isFinite(state) && !newFormalArguments.contains(Context.getSetFor(setDescription.key)))
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

    Context.setMethodContext(DefaultSilverConverter.prog, method)

    // TODO: just catching the exception is a hack, better perform a search through the program if there exists a field access with a function as receiver
//    val aliasAnalysisResult: Option[CfgResult[SimpleAliasAnalysisState]] =
//      try {
//        val aliasEntry = aliasAnalysisBuilder.build(program, method)
//        val aliasInterpreter = FinalResultForwardInterpreter[SimpleAliasAnalysisState]()
//        val aliasResult = aliasInterpreter.execute(method.body, aliasEntry)
//        Some(aliasResult)
//      } catch {
//        case _: IllegalArgumentException =>
//          logger.warn("Heap analysis failed!")
//          None
//      }

    Context.setAliases(method.name.name.toString, None)

    val numericalEntry = numericalStateBuilder.build(program, method)
    val numericalInterpreter = FinalResultForwardInterpreter[NumericalStateType]()
    val numericalResult = numericalInterpreter.execute(method.body, numericalEntry)

    Context.setNumericalInfo(method.name.name, numericalResult)

    val quantifiedPermissionsEntry = QuantifiedPermissionsState()
    val quantifiedPermissionsInterpreter = new QPBackwardInterpreter
    val quantifiedPermissionsResult = quantifiedPermissionsInterpreter.execute(method.body, quantifiedPermissionsEntry)

    quantifiedPermissionsResult
  }
}