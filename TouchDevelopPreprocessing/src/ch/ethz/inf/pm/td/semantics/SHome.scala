
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
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
  val field_players = new TouchField("players",TMedia_Player_Collection.typName)

  /** Gets the printers on the current wireless network */
  val field_printers = new TouchField("printers",TPrinter_Collection.typName)

  /** Gets the media servers on the home network */
  val field_servers = new TouchField("servers",TMedia_Server_Collection.typName)

  val typName = "Home"
  val typ = DefaultTouchType(typName,isSingleton = true, fields = List(field_players,field_printers,field_servers))

}

class SHome extends AAny {

  def getTyp = SHome.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Choose a media player on the current wireless network */
    case "choose player" =>
      val ret = If[S]( CollectionSize[S](Field[S](this0,SHome.field_players)) equal 0, Then = {
        Return[S](Invalid(TMedia_Player.typ))(_,pp)
      }, Else = {
        Return[S](CollectionSummary[S](Field[S](this0,SHome.field_players)))(_,pp)
      })
      ret

    /** Choose a printer on the current wireless network */
    case "choose printer" =>
      val ret = If[S]( CollectionSize[S](Field[S](this0,SHome.field_printers)) equal 0, Then = {
        Return[S](Invalid(TPrinter.typ))(_,pp)
      }, Else = {
        Return[S](CollectionSummary[S](Field[S](this0,SHome.field_printers)))(_,pp)
      })
      ret

    /** Choose a media server on the current wireless network */
    case "choose server" =>
      val ret = If[S]( CollectionSize[S](Field[S](this0,SHome.field_servers)) equal 0, Then = {
        Return[S](Invalid(TMedia_Server.typ))(_,pp)
      }, Else = {
        Return[S](CollectionSummary[S](Field[S](this0,SHome.field_servers)))(_,pp)
      })
      ret

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
