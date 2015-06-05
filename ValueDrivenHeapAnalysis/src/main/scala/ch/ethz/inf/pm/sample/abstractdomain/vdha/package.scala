package ch.ethz.inf.pm.sample.abstractdomain

import scala.annotation.elidable
import scala.annotation.elidable._

package object vdha {
  trait GlbPreservingIdsStrategy {
    def apply[S <: SemanticDomain[S]](left: S, right: S): S
  }

  object DefaultGlbPreservingIdsStrategy extends GlbPreservingIdsStrategy {
    def apply[S <: SemanticDomain[S]](left: S, right: S): S = {
      val newRightIds = left.edgeLocalAndAccessPathIds diff right.ids.getNonTop
      val newLeftIds = right.edgeLocalAndAccessPathIds diff left.ids.getNonTop
      val newLeft = left.createVariables(newLeftIds)
      val newRight = right.createVariables(newRightIds)
      newLeft.glb(newRight)
    }
  }

  // Ugly hack to make it possible to replace the behavior of
  // ExtendedSemanticDomain.glbPreservingIds.
  // See CustomGlbPreservingIdsStrategy for more details.
  // TODO: Find a more elegant solution. Most importantly, avoid global state.
  var glbPreservingIdsStrategy: GlbPreservingIdsStrategy =
    DefaultGlbPreservingIdsStrategy

  def withGlbPreservingIdsStrategy[A](strategy: GlbPreservingIdsStrategy, f: () => A): A = {
    val oldStrategy = glbPreservingIdsStrategy
    glbPreservingIdsStrategy = strategy
    val result = f()
    glbPreservingIdsStrategy = oldStrategy
    result
  }

  /** Wraps [[Predef.require(r)]], making calls to it elidable
    * with the compiler option '-Xelide-below OFF'.
    */
  @elidable(ASSERTION) @inline
  def require(requirement: Boolean) {
    Predef.require(requirement)
  }

  /** Wraps [[Predef.require(r, m)]], making calls to it elidable
    * with the compiler option '-Xelide-below OFF'.
    */
  @elidable(ASSERTION) @inline
  def require(requirement: Boolean, message: => Any) {
    Predef.require(requirement, message)
  }

  /**
   * Replaces all non-numerical `VariableIdentifier`s in the given expression
   * with a corresponding `AccessPathIdentifier`.
   *
   * @todo replace all `VariableIdentifiers`, including numerical ones
   */
  def normalizeExpression(exp: Expression): Expression =
    exp.transform({
      case v: VariableIdentifier if v.typ.isObject => AccessPathIdentifier(v)
      case e => e
    })

  /** Add various helper methods to `SemanticDomain` objects. */
  implicit class ExtendedSemanticDomain[S <: SemanticDomain[S]](state: S) {
    /** Returns all variable identifiers in the state. */
    def variableIds: Set[VariableIdentifier] =
      state.ids.getNonTop.collect({ case id: VariableIdentifier => id })

    /** Returns all value heap identifiers in the state. */
    def valueHeapIds: Set[ValueHeapIdentifier] =
      state.ids.getNonTop.collect({ case id: ValueHeapIdentifier => id })

    /** Returns all value heap identifiers of a certain vertex in the state. */
    def valueHeapIds(vertex: Vertex): Set[ValueHeapIdentifier] =
      valueHeapIds.filter(_.obj == vertex)

    /** Returns all edge-local identifiers in the state. */
    def edgeLocalIds: Set[EdgeLocalIdentifier] =
      state.ids.getNonTop.collect({ case id: EdgeLocalIdentifier => id })

    /** Returns all source edge-local identifiers in the state. */
    def sourceEdgeLocalIds: Set[EdgeLocalIdentifier] =
      edgeLocalIds.filter(_.isForSource)

    /** Returns all target edge-local identifiers in the state. */
    def targetEdgeLocalIds: Set[EdgeLocalIdentifier] =
      edgeLocalIds.filter(_.isForTarget)

    /** Returns all target edge-local identifiers for the given field. */
    def targetEdgeLocalIds(field: Option[String]): Set[EdgeLocalIdentifier] =
      targetEdgeLocalIds.filter(_.accPath == List(field))

    /** Returns all access path identifiers in th state. */
    def accPathIds: Set[AccessPathIdentifier] =
      state.ids.getNonTop.collect({ case id: AccessPathIdentifier => id })

    /** Returns all edge-local and access path identifiers in the state. */
    def edgeLocalAndAccessPathIds: Set[Identifier] =
      state.ids.getNonTop.filter(id => id.isInstanceOf[EdgeLocalIdentifier] ||
        id.isInstanceOf[AccessPathIdentifier])

    /** Returns the GLB of this and another state, but takes the union
      * of their identifiers.
      */
    def glbPreserveIds(other: S): S =
      glbPreservingIdsStrategy.apply(state, other)

    /** Removes all access path identifiers from the state. */
    def removeAccessPathIds(): S =
      state.removeVariables(accPathIds)
  }
}
