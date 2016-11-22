/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Printer
 *
 * A printer on the home network
 *
 * @author Lucas Brutschy
 */

trait Default_TPrinter extends AAny {

  lazy val typeName = TypeName("Printer")
          
  /** Never used: [**not implemented**] [**obsolete**] Gets the detailled information about this device */
  def member_device = ApiMember(
    name = "device",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDevice,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Indicates if new jobs can start processing immediately without waiting. */
  def member_is_idle = ApiMember(
    name = "is idle",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Indicates if jobs are processing; new jobs will wait before processing, i.e., are said to be pending. */
  def member_is_processing = ApiMember(
    name = "is processing",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Indicates if no jobs can be processed and intervention is needed. */
  def member_is_stopped = ApiMember(
    name = "is stopped",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Gets the name of the printer */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Queues a job to print the text. */
  def member_print_text = ApiMember(
    name = "print text",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Indicates additional information about why the Printer is in its current state. */
  def member_state_reason = ApiMember(
    name = "state reason",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "device" -> member_device,
    "is idle" -> member_is_idle,
    "is processing" -> member_is_processing,
    "is stopped" -> member_is_stopped,
    "name" -> member_name,
    "print text" -> member_print_text,
    "state reason" -> member_state_reason
  )
            

}
          
