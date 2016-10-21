package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.MethodDeclaration
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverInferenceRunner, TopType}
import ch.ethz.inf.pm.sample.{AnalysisUnitContext, StdOutOutput, SystemParameters}

/**
  * @author Severin Münger
  *         Added on 19/10/16.
  */
object Main {
  def main(args: Array[String]) = {

    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()

    QuantifiedPermissionsAnalysisRunner.main(args)
  }
}

/** SIL analysis runner that uses the default QuantifiedPermissions analysis. */
object QuantifiedPermissionsAnalysisRunner extends SilverInferenceRunner[QuantifiedPermissionsState] {
  SystemParameters.isValueDrivenHeapAnalysis = false
  SystemParameters.typ = TopType

  val analysis = ForwardAndBackwardAnalysis[QuantifiedPermissionsState](QuantifiedPermissionsState.QuantifiedPermissionsEntryStateBuilder[Apron.Polyhedra, QuantifiedPermissionsState], new DefaultQuantifiedPermissionsState())
}

case class AnalysisResult[S <: State[S]](method: MethodDeclaration, cfgState: TrackingCFGState[S]) {}

case class ForwardAndBackwardAnalysis[S <: State[S]](entryStateBuilder: EntryStateBuilder[S], defaultExitState: S) extends Analysis[S] {

  def analyze(method: MethodDeclaration): AnalysisResult[S] =
    analyze(method, entryStateBuilder.build(method))

  def analyze(method: MethodDeclaration, entryState: S): AnalysisResult[S] = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(method)) {
      val interpreter = TrackingForwardInterpreter[S](entryState)
      val forwardStates = interpreter.forwardExecute(method.body, entryState)
      AnalysisResult(method, forwardStates)

    }
  }
}