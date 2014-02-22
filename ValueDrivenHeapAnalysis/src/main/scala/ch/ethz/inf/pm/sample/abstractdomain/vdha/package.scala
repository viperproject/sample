package ch.ethz.inf.pm.sample.abstractdomain

import scala.annotation.elidable
import scala.annotation.elidable._

package object vdha {
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
    /** Returns all edge-local identifiers in the state. */
    def edgeLocalIds: Set[EdgeLocalIdentifier] =
      state.ids.collect({ case id: EdgeLocalIdentifier => id })

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
      state.ids.collect({ case id: AccessPathIdentifier => id })

    /** Returns all edge-local and access path identifiers in the state. */
    def edgeLocalAndAccessPathIds: Set[Identifier] =
      state.ids.filter(id => id.isInstanceOf[EdgeLocalIdentifier] ||
        id.isInstanceOf[AccessPathIdentifier])

    /** Returns the GLB of this and another state, but takes the union
      * of their identifiers.
      */
    def glbPreserveIds(right: S): S = {
      val newRightIds = state.edgeLocalAndAccessPathIds diff right.ids
      val newLeftIds = right.edgeLocalAndAccessPathIds diff state.ids
      val newLeft = state.createVariables(newLeftIds)
      val newRight = right.createVariables(newRightIds)
      newLeft.glb(newRight)
    }

    /** Removes all access path identifiers from the state. */
    def removeAccessPathIds(): S =
      state.removeVariables(accPathIds)
  }
}
