
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
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
  val typ = new TouchType(typName,isSingleton = true,List(field_players,field_printers,field_servers))

}

class SHome extends AAny {

  def getTyp = SHome.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Choose a media player on the current wireless network */
    case "choose_player" =>
      If[S]( CollectionSize[S](Field[S](this0,SHome.field_players)) equal 0, Then = {
        Return[S](Invalid(TMedia_Player.typ))(_,pp)
      }, Else = {
        Return[S](CollectionSummary[S](Field[S](this0,SHome.field_players)))(_,pp)
      })

    /** Choose a printer on the current wireless network */
    case "choose_printer" =>
      If[S]( CollectionSize[S](Field[S](this0,SHome.field_printers)) equal 0, Then = {
        Return[S](Invalid(TPrinter.typ))(_,pp)
      }, Else = {
        Return[S](CollectionSummary[S](Field[S](this0,SHome.field_printers)))(_,pp)
      })

    /** Choose a media server on the current wireless network */
    case "choose_server" =>
      If[S]( CollectionSize[S](Field[S](this0,SHome.field_servers)) equal 0, Then = {
        Return[S](Invalid(TMedia_Server.typ))(_,pp)
      }, Else = {
        Return[S](CollectionSummary[S](Field[S](this0,SHome.field_servers)))(_,pp)
      })

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
