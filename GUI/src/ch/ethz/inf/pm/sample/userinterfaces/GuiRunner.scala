package ch.ethz.inf.pm.sample.userinterfaces

import ch.ethz.inf.pm.sample.abstractdomain.{Analysis, State}
import ch.ethz.inf.pm.sample.property.OutputCollector
import scala.collection.immutable.List

/**
 * Created with IntelliJ IDEA.
 * User: lucas
 * Date: 11/22/13
 * Time: 1:11 PM
 * To change this template use File | Settings | File Templates.
 */
object GuiRunner {

  def run[S <: State[S]](a:Analysis,methods:List[String],state:S,o:OutputCollector) {
    ShowGraph.Show[S](a.analyze(methods,state,o))
  }

}
