
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of home
 *
 * Interact with devices in the home network. Devices must be UPnPâ„¢ compatible.
 *
 * @author Lucas Brutschy
 */ 

object SHome {

  val typName = "home"
  val typ = TouchType(typName,isSingleton = true,List())

}

class SHome extends AAny {

  def getTyp = SHome.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Choose a media player on the current wireless network */
    // case "choose_player" => 
    //   Return[S](Valid(TMedia_Player.typ))
    // DECLARATION AS FIELD: 
    //   /** Choose a media player on the current wireless network */
    //   val field_choose_player = new TouchField("choose_player",TMedia_Player.typ)

    /** Choose a printer on the current wireless network */
    // case "choose_printer" => 
    //   Return[S](Valid(TPrinter.typ))
    // DECLARATION AS FIELD: 
    //   /** Choose a printer on the current wireless network */
    //   val field_choose_printer = new TouchField("choose_printer",TPrinter.typ)

    /** Choose a media server on the current wireless network */
    // case "choose_server" => 
    //   Return[S](Valid(TMedia_Server.typ))
    // DECLARATION AS FIELD: 
    //   /** Choose a media server on the current wireless network */
    //   val field_choose_server = new TouchField("choose_server",TMedia_Server.typ)

    /** Gets the media players on the current wireless network */
    // case "players" => 
    //   Return[S](Valid(TMedia_Player_Collection.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the media players on the current wireless network */
    //   val field_players = new TouchField("players",TMedia_Player_Collection.typ)

    /** Gets the printers on the current wireless network */
    // case "printers" => 
    //   Return[S](Valid(TPrinter_Collection.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the printers on the current wireless network */
    //   val field_printers = new TouchField("printers",TPrinter_Collection.typ)

    /** Gets the media servers on the home network */
    // case "servers" => 
    //   Return[S](Valid(TMedia_Server_Collection.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the media servers on the home network */
    //   val field_servers = new TouchField("servers",TMedia_Server_Collection.typ)

    // FIELDS: , field_choose_player, field_choose_printer, field_choose_server, field_players, field_printers, field_servers

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
