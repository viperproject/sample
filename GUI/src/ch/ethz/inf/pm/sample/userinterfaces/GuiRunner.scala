package ch.ethz.inf.pm.sample.userinterfaces

import ch.ethz.inf.pm.sample.abstractdomain.{Analysis, State}
import ch.ethz.inf.pm.sample.property.OutputCollector
import scala.collection.immutable.List

/**
 * This is used to bridge the type-checking between Analyses and ShowGraph (both written in Scala)
 * to be used from inside Java code
 */
object GuiRunner {

  def run[S <: State[S]](a:Analysis,methods:List[String],state:S,o:OutputCollector) {
    ShowGraph.Show[S](a.analyze(methods,state,o))
  }

}