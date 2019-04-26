/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.oorepresentation.sil

object Constants {
  // Prefix for symbols that do not exist in the original SIL program.
  // Ensures that the resulting identifier is not a valid SIL identifier.
  val GhostSymbolPrefix = "☐"

  // Sample variable that holds the resulting value of a SIL function.
  // Since 'result' is a keyword in SIL, there is no risk of name clashes
  // when using 'result'.
  val ResultVariableName = "result"
}