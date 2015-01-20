package ch.ethz.inf.pm.sample.userinterfaces

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.property.OutputCollector
import scala.collection.immutable.List
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.SystemParameters

/**
 * This is used to bridge the type-checking between Analyses and ShowGraph (both written in Scala)
 * to be used from inside Java code
 */
object GuiRunner {

  def run[S <: State[S]](a:Analysis,methods:List[String],state:S,o:OutputCollector) {
    ShowGraph.Show[S](a.analyze(methods,state,o))
  }

  def createEmptyValue(): ExpressionSet = new ExpressionSet(SystemParameters.getType.top, SetDomain.Default())

  def createNonRelationalMayHeapDomain[I <: NonRelationalHeapIdentifier[I]](id: I): NonRelationalHeapDomain[I] = {
    val ids: MaybeHeapIdSetDomain[I] = new MaybeHeapIdSetDomain[I]
    val env: VariableEnv[I] = new VariableEnv[I](ids)
    val heap: HeapEnv[I] = new HeapEnv[I](ids)
    new NonRelationalHeapDomain[I](env, heap, ids, id)
  }

}