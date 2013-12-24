import ch.ethz.inf.pm.sample.abstractdomain.SemanticDomain

package object graph {
  type Path[S <: SemanticDomain[S]] = List[EdgeWithState[S]]
}
