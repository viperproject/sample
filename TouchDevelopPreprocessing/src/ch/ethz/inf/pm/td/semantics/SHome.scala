
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of home
 *
 * Interact with devices in the home network. Devices must be UPnPâ„¢ compatible.
 *
 * TODO: Here we define players statically. Does that even make sense?
 * Maybe move the collections to the environment
 *
 * @author Lucas Brutschy
 */

object SHome extends ASingleton {

  /** Gets the media players on the current wireless network */
  lazy val field_players = new ApiField("players", TMedia_Player_Collection.typeName)

  /** Gets the printers on the current wireless network */
  lazy val field_printers = new ApiField("printers", TPrinter_Collection.typeName)

  /** Gets the media servers on the home network */
  lazy val field_servers = new ApiField("servers", TMedia_Server_Collection.typeName)

  lazy val typeName = TypeName("Home")

  override def possibleFields = super.possibleFields ++ List(field_players, field_printers, field_servers)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Choose a media player on the current wireless network */
    case "choose player" =>
      val ret = If[S](TMedia_Player_Collection.collectionSize[S](Field[S](this0, SHome.field_players)) equal 0, Then = {
        Return[S](Invalid(TMedia_Player, "user may abort the player selection"))(_, pp)
      }, Else = {
        Return[S](TPrinter_Collection.collectionAllValues[S](Field[S](this0, SHome.field_players)))(_, pp)
      })
      ret

    /** Choose a printer on the current wireless network */
    case "choose printer" =>
      val ret = If[S](TPrinter_Collection.collectionSize[S](Field[S](this0, SHome.field_printers)) equal 0, Then = {
        Return[S](Invalid(TPrinter, "user may abort the printer selection"))(_, pp)
      }, Else = {
        Return[S](TPrinter_Collection.collectionAllValues[S](Field[S](this0, SHome.field_printers)))(_, pp)
      })
      ret

    /** Choose a media server on the current wireless network */
    case "choose server" =>
      val ret = If[S](TMedia_Server_Collection.collectionSize[S](Field[S](this0, SHome.field_servers)) equal 0, Then = {
        Return[S](Invalid(TMedia_Server, "user may abort the media server selection"))(_, pp)
      }, Else = {
        Return[S](TPrinter_Collection.collectionAllValues[S](Field[S](this0, SHome.field_servers)))(_, pp)
      })
      ret

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
