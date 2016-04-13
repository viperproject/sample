/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample

import ch.ethz.inf.pm.sample.oorepresentation.{ControlFlowGraph, MethodDeclaration, Type}

case class AnalysisUnitContext(method: MethodDeclaration) {

  // legacy helpers
  def methodName: String = if (method != null)  method.name.toString else null
  def cfg: ControlFlowGraph = method.body
  def clazzType: Type = method.classDef.typ
  def clazz = method.classDef
}
