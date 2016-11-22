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
 * Specifies the abstract semantics of Home
 *
 * Interact with devices in the home network. Devices must be UPnP™ compatible.
 *
 * @author Lucas Brutschy
 */

trait Default_SHome extends ASingleton {

  lazy val typeName = TypeName("Home", isSingleton = true)
          
  /** Rarely used: [**not implemented**] [**obsolete**] Choose a media player on the current wireless network */
  def member_choose_player = ApiMember(
    name = "choose player",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMedia_Player,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Choose a printer on the current wireless network */
  def member_choose_printer = ApiMember(
    name = "choose printer",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPrinter,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] [**obsolete**] Choose a media server on the current wireless network */
  def member_choose_server = ApiMember(
    name = "choose server",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMedia_Server,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] [**obsolete**] Gets the media players on the current wireless network */
  def member_players = ApiMember(
    name = "players",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Player),
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Gets the printers on the current wireless network */
  def member_printers = ApiMember(
    name = "printers",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TPrinter),
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] [**obsolete**] Gets the media servers on the home network */
  def member_servers = ApiMember(
    name = "servers",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Server),
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "choose player" -> member_choose_player,
    "choose printer" -> member_choose_printer,
    "choose server" -> member_choose_server,
    "players" -> member_players,
    "printers" -> member_printers,
    "servers" -> member_servers
  )
            

}
          
