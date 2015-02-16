
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SHome
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

object SHome extends Default_SHome {

  /** Gets the media players on the current wireless network */
  lazy val field_players = ApiField("players", GCollection(TMedia_Player))

  /** Gets the printers on the current wireless network */
  lazy val field_printers = ApiField("printers", GCollection(TPrinter))

  /** Gets the media servers on the home network */
  lazy val field_servers = ApiField("servers", GCollection(TMedia_Server))

  override def possibleFields = super.possibleFields ++ List(field_players, field_printers, field_servers)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Choose a media player on the current wireless network */
    case "choose player" =>
      val ret = If[S](GCollection(TMedia_Player).collectionSize[S](Field[S](this0, SHome.field_players)) equal 0, Then = {
        Return[S](Invalid(TMedia_Player, "user may abort the player selection"))(_, pp)
      }, Else = {
        Return[S](GCollection(TPrinter).collectionAllValues[S](Field[S](this0, SHome.field_players)))(_, pp)
      })
      ret

    /** Choose a printer on the current wireless network */
    case "choose printer" =>
      val ret = If[S](GCollection(TPrinter).collectionSize[S](Field[S](this0, SHome.field_printers)) equal 0, Then = {
        Return[S](Invalid(TPrinter, "user may abort the printer selection"))(_, pp)
      }, Else = {
        Return[S](GCollection(TPrinter).collectionAllValues[S](Field[S](this0, SHome.field_printers)))(_, pp)
      })
      ret

    /** Choose a media server on the current wireless network */
    case "choose server" =>
      val ret = If[S](GCollection(TMedia_Server).collectionSize[S](Field[S](this0, SHome.field_servers)) equal 0, Then = {
        Return[S](Invalid(TMedia_Server, "user may abort the media server selection"))(_, pp)
      }, Else = {
        Return[S](GCollection(TPrinter).collectionAllValues[S](Field[S](this0, SHome.field_servers)))(_, pp)
      })
      ret

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
