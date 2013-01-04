
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of home
 *
 * Interact with devices in the home network. Devices must be UPnPâ„¢ compatible.
 *
 * TODO: Here we define players statically. Does that even make sense?
 *       Maybe move the collections to the environment
 *
 * @author Lucas Brutschy
 */ 

object SHome {

  /** Gets the media players on the current wireless network */
  val field_players = new TouchField("players",TMedia_Player_Collection.typ)

  /** Gets the printers on the current wireless network */
  val field_printers = new TouchField("printers",TPrinter_Collection.typ)

  /** Gets the media servers on the home network */
  val field_servers = new TouchField("servers",TMedia_Server_Collection.typ)

  val typName = "home"
  val typ = TouchType(typName,isSingleton = true,List(field_printers,field_servers))

}

class SHome extends AAny {

  def getTyp = SHome.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Choose a media player on the current wireless network */
    case "choose_player" =>
      Error[S](CollectionSize[S](Field[S](this0,SHome.field_players)) == 0,
        "The list of players the user choses from may be empty. Check if there are players in the network first")
      Return[S](CollectionAt)

    /** Choose a printer on the current wireless network */
    case "choose_printer" =>
      Error[S](CollectionSize[S](Field[S](this0,SHome.field_printers)) == 0,
        "The list of printers the user choses from may be empty. Check if there are printers in the network first")
      Return[S](Valid(TPrinter.typ))

    /** Choose a media server on the current wireless network */
    case "choose_server" =>
      Error[S](CollectionSize[S](Field[S](this0,SHome.field_printers)) == 0,
        "The list of servers the user choses from may be empty. Check if there are servers in the network first")
      Return[S](Valid(TMedia_Server.typ))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
