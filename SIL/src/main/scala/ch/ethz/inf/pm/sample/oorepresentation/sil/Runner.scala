package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.execution.{AnalysisResult, SimpleAnalysis, AnalysisRunner}
import ch.ethz.inf.pm.sample.abstractdomain._
import semper.sil.ast.Program

trait SilAnalysisRunner[S <: State[S]] extends AnalysisRunner[S] {
  val compiler = new SilCompiler()

  def run(program: Program): List[AnalysisResult[S]] = {
    compiler.compileProgram(program)
    _run()
  }
}

object DefaultAnalysisRunner extends SilAnalysisRunner[ValueDrivenHeapState.Default[ApronInterface.Default]] {
  val analysis = SimpleAnalysis[ValueDrivenHeapState.Default[ApronInterface.Default]](DefaultHeapEntryStateBuilder)

  override def toString = "Default Analysis"
}

object PreciseAnalysisRunner extends SilAnalysisRunner[PreciseValueDrivenHeapState.Default[ApronInterface.Default]] {
  val analysis = SimpleAnalysis[PreciseValueDrivenHeapState.Default[ApronInterface.Default]](PreciseHeapEntryStateBuilder)

  override def toString = "Precise Analysis"
}