/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
import ch.ethz.inf.pm.td.defsemantics.{Default_TWeb_Event_Source, Default_TWeb_Request}
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Web Event Source
 *
 * A Server-Sent-Events client
 *
 * @author Lucas Brutschy
 */ 

object TWeb_Event_Source extends Default_TWeb_Event_Source {

  lazy val field_state = ApiField("state", TString)

  override lazy val possibleFields = super.possibleFields ++ Set(
    field_state
  )

}
      
