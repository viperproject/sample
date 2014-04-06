package ch.ethz.inf.pm.td.analysis.interpreter

import ch.ethz.inf.pm.sample.{StringCollector, StdOutOutput, SystemParameters}
import ch.ethz.inf.pm.td.compiler.{TouchType, TouchSingletonProgramPoint, TypeList, TouchCompiler}
import ch.ethz.inf.pm.td.analysis.backward.InterpreterTestInput
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

object ConcreteRunner {

  def runScriptFile(file: String): Seq[SampleMessage] = {
    SystemParameters.resetOutput()
    SystemParameters.analysisOutput = if (TouchAnalysisParameters.silent) new StringCollector() else new StdOutOutput()
    SystemParameters.progressOutput =if (TouchAnalysisParameters.silent) new StringCollector() else new StdOutOutput()

    val compiler = new TouchCompiler
    SystemParameters.compiler = compiler // would be nice if we could get rid of this - not realistic now
    compiler.reset()
    compiler.compile(file)
    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodsSemantics())

    val mainAction = compiler.getPublicMethods find (_.name.toString == "main") getOrElse {
      sys.error("No main method found in script")
    }
    val concreteState = new ConcreteInterpreterState()

    for (sv <- compiler.singletonVars) {
      val singletonRef = concreteState.createObject(sv.typ.asInstanceOf[TouchType])
      concreteState.setGlobal(sv, singletonRef)
    }


    def defaultGlobalValue(varId: VariableIdentifier): TouchValue = {
      varId.typ.name match {
        case "Number" => NumberV(0)
        case "Boolean" => BooleanV(v = false)
        case _ => InvalidV(varId.typ)
      }
    }

    // set global user data
    for (globalDecl <- compiler.globalData) {
      val varId = compiler.resolveGlobalData(globalDecl.variable.getName)
      concreteState.setGlobal(varId, defaultGlobalValue(varId))
    }

    val interpreterInput = InterpreterTestInput(
      startMethod = mainAction,
      startMethodActualParams = ActualMethodParams(),
      initialState = concreteState
    )
    val interpreter = new ConcreteInterpreter(compiler, interpreterInput)
    val result = interpreter.run()
    result match {
      case FailedExecution(sampleErr) => Reporter.reportError(sampleErr)
      case _ =>
    }

    (Reporter.seenErrors ++ Reporter.seenInfos).toSeq
  }

  def main(args: Array[String]) {
    assert (args.size == 1, "ConcreteRunner takes exactly 1i argument with .td script to run")
    val file = args(0)
    runScriptFile(file)
  }
}

