/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.util

import viper.silver.{ast => sil}

/**
  * Some utility functions for silver expressions.
  *
  * @author Jerome Dohrau
  */
object SilverExpressions {
  /**
    * An expression representing a true constant.
    */
  val tt: sil.BoolLit = boolean(value = true)

  /**
    * An expression representing a false constant.
    */
  val ff: sil.BoolLit = boolean(value = false)

  /**
    * Returns an expression representing a boolean constant with the given
    * value.
    *
    * @param value    The value.
    * @param position The position.
    * @return The boolean constant.
    */
  def boolean(value: Boolean, position: sil.Position = sil.NoPosition): sil.BoolLit =
    if (value) sil.TrueLit()(position)
    else sil.FalseLit()(position)

  def and(left: sil.Exp, right: sil.Exp, position: sil.Position = sil.NoPosition): sil.Exp =
    sil.And(left, right)(position)

  val none: sil.NoPerm = sil.NoPerm()()

  val write: sil.FullPerm = sil.FullPerm()()
}
