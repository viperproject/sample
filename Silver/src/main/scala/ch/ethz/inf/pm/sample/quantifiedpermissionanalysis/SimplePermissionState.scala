package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, ExpressionSet, Lattice, SimpleState}


/** Trait adding Inhale/Exhale methods to a SimpleState.
  *
  * @author Caterina Urban
  */
trait SimplePermissionState[S <: SimplePermissionState[S]] extends SimpleState[S] {
  this: S =>

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  def inhale(acc: Expression): S

  def inhale(acc: ExpressionSet): S = unlessBottom(acc, {
    Lattice.bigLub(acc.toSetOrFail.map(inhale))
  })

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  def exhale(acc: Expression): S

  def exhale(acc: ExpressionSet): S = unlessBottom(acc, {
    Lattice.bigLub(acc.toSetOrFail.map(exhale))
  })
}