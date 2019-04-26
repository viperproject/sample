/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.defsemantics.Default_TWeb_Socket

/**
 * Customizes the abstract semantics of Web Socket
 *
 * A web socket
 *
 * @author Lucas Brutschy
 */

object TWeb_Socket extends Default_TWeb_Socket {
          

//  lazy val field_receive = ApiField("receive",TWeb_Socket_Message)
//  lazy val field_ready_state = ApiField("ready state",TString)
//  lazy val field_buffered_amount = ApiField("buffered amount",TNumber)
                  

//  override lazy val possibleFields = super.possibleFields ++ Set(
//    field_receive,
//    field_ready_state,
//    field_buffered_amount
//  )
                  

}
          
