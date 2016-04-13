/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, MethodDeclaration, ProgramPoint}

case class AbstractErrorInfo[S <: State[S]](pp: ProgramPoint, method: MethodDeclaration, errorExpr: ExpressionSet, state: S, errorState: S, cfgPosition: CFGPosition)
