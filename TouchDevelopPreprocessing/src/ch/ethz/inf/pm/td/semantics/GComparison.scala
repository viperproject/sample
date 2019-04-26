/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.defsemantics.Default_GComparison
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Customizes the abstract semantics of Comparison
 *
 * An atomic comparison action
 *
 * @author Lucas Brutschy
 */

case class GComparison (TElt:AAny) extends Default_GComparison
          
