package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * Super-class of all singletons
 */
trait ASingleton extends AAny {

  override def isSingleton = true

}
