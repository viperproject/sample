/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{ArithmeticOperator, BinaryArithmeticExpression, Expression, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, IntType}
import viper.silicon.Silicon
import viper.silver.verifier.Success
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 16/11/16.
  */
object ProgramUtils {

  def isFunctionInjective(function: sil.FuncLike, expr: Expression, numericalInfo: Context.NumericalDomainType, program: sil.Program = Context.program): Boolean = {
    val intDecls = Context.getQuantifiedVarDeclsForType(sil.Int, 2)
    val (i1, i2) = (intDecls.head, intDecls.last)
    val formalArgs = function.formalArgs.filter(formalArg => formalArg.typ != sil.Int) ++ intDecls
    val i1Id = VariableIdentifier(i1.name)(IntType)
    val i2Id = VariableIdentifier(i2.name)(IntType)
    val expressionToAssume1 = BinaryArithmeticExpression(i1Id, expr, ArithmeticOperator.==)
    val expressionToAssume2 = BinaryArithmeticExpression(i2Id, expr, ArithmeticOperator.==)
    val constraints1 = numericalInfo.createVariable(i1Id).assume(expressionToAssume1).removeVariables(numericalInfo.ids.getNonTop).getConstraints(Set(i1Id))
    val constraints2 = numericalInfo.createVariable(i2Id).assume(expressionToAssume2).removeVariables(numericalInfo.ids.getNonTop).getConstraints(Set(i2Id))
    val precondition =
      (Seq(sil.NeCmp(i1.localVar, i2.localVar)()) ++
      constraints1.map(expr => DefaultSampleConverter.convert(expr)) ++
      constraints2.map(expr => DefaultSampleConverter.convert(expr))).reduce((left, right) => sil.And(left, right)())
    val postcondition = sil.NeCmp(
      sil.FuncLikeApp(function, function.formalArgs.map(formalArg => if (formalArg.typ == sil.Int) i1.localVar else formalArg.localVar), Map()),
      sil.FuncLikeApp(function, function.formalArgs.map(formalArg => if (formalArg.typ == sil.Int) i2.localVar else formalArg.localVar), Map()))()
    val methodToCheck = sil.Method(Context.createNewUniqueFunctionIdentifier("injectivity_check"), formalArgs, Seq(), Seq(precondition), Seq(postcondition), Seq(), sil.Seqn(Seq())())()
    val newProgram: sil.Program = sil.Program(program.domains, program.fields, program.functions, program.predicates, Seq(methodToCheck))()
    println(newProgram)
    val silicon = new Silicon(Seq(("startedBy", "viper.silicon.SiliconTests")))
    silicon.parseCommandLine(Seq("dummy.sil"))
    silicon.config.initialize { case _ => silicon.config.initialized = true }
    silicon.start()
    silicon.verify(newProgram) match {
      case Success => true
      case _ => false
    }
  }

}
