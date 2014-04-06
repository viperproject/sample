package ch.ethz.inf.pm.td.analysis.backward

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, MethodDeclaration}
import ch.ethz.inf.pm.td.analysis.interpreter._
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.td.analysis.backward.InterpreterTestInput
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.td.analysis.interpreter.RefV
import ch.ethz.inf.pm.td.analysis.backward.InterpreterTestInput
import ch.ethz.inf.pm.td.analysis.interpreter.NumberV
import ch.ethz.inf.pm.td.analysis.interpreter.BooleanV
import ch.ethz.inf.pm.td.analysis.interpreter.StringV
import ch.ethz.inf.pm.td.analysis.interpreter.ActualMethodParams
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.td.compiler.CFGGenerator

/** Contais the complete input for a concrete testing run of a method */
case class InterpreterTestInput(
                                /** Method to test */
                                startMethod: MethodDeclaration,
                                /** Method parameter values */
                                startMethodActualParams:ActualMethodParams,
                                /** Initial concrete state (heap for objects etc.) */
                                initialState: ConcreteInterpreterState,
                                /** Variables for all singletons (allocated on the heap) */
                                singletonVars: Set[VariableIdentifier] = Set.empty,
                                /** Variables for global data (allocated on concrete heap) */
                                globalUserData: Set[VariableIdentifier] = Set.empty,
                                /** Concrete values provided during non-deterministic accesses */
                                nonDetInputs: Map[ProgramPoint, TouchValue] = Map.empty,
                                /** Seed for pseudo-random decisions (for those we dont have nondet ids) */
                                nonDetRandomSeed: Long = 0) {

  def nonDetInputAt(pp: ProgramPoint): Option[TouchValue] = {
    val nd = nonDetInputs.get(pp)
    if (!nd.isDefined) {
      println("WARNING: nondet input not defined")
    }
    nd
  }
}

/**
 * A simple pretty-printer for concrete test inputs.
 *
 * Please note that this functionality is experimental. Inspect the concrete
 * state or refined abstract entry states (debugger breakpoints...) in case
 * something seems to be missing.
 *
 */
object CounterExamplePrettyPrinter {
  def print(input: InterpreterTestInput): String = {
    val out = new StringBuilder
    val concreteState = input.initialState

    out.append("---------[ Test input ]---------------------------\n")
    out.append(s"Entry method: ${input.startMethod.name}\n")
    out.append("Method parameters:\n")
    for ((varId, touchVal) <- input.startMethodActualParams.values) {
      out.append((" " * 4) + varId.getName + " => " + concreteState.printTouchValue(touchVal) + "\n")
    }

    out.append("Non-deterministic inputs:\n")
    for ((pp, touchVal) <- input.nonDetInputs) {
      out.append((" " * 4) + pp + " => " + concreteState.printTouchValue(touchVal) + "\n")
    }

    out.append("Global user-defined data:\n")
    for (varId <- input.globalUserData) {
      // non-determinism state already printed above
      if (!CFGGenerator.isNonDetIdent(varId.getName)) {
        val touchVal = concreteState.getVar(varId)
        out.append((" " * 4) + varId.getName + " => " + concreteState.printTouchValue(touchVal) + "\n")
      }
    }

    out.append("State of relevant singletons\n")
    for (singletonVar <- input.singletonVars) {
      val touchRef = concreteState.getVar(singletonVar).asInstanceOf[RefV]
      val touchObj = concreteState.getObject(touchRef)
      if (!touchObj.fields.isEmpty) {
        out.append((" " * 4) + singletonVar.getName + " => " + concreteState.printTouchValue(touchRef) + "\n")
      }
    }

    out.append("---------[ End of test input]---------------------\n\n")

    out.mkString
  }
}
