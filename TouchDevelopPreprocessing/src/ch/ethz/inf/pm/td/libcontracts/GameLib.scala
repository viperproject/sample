package ch.ethz.inf.pm.td.libcontracts

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichExpressionImplicits
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.semantics.TBoolean

/**
 * @author Lucas Brutschy
 */
case object GameLib extends LibraryContract with RichExpressionImplicits {

  override val name = "game"

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet],
                                               returnedType: TouchType)(implicit pp: ProgramPoint, state: S) = {
    method match {

      case "add life" =>
        Top[S](returnedType)

      case "add score" =>
        Top[S](returnedType)

      case "board" =>
        Top[S](returnedType)

      case "bounce on sides" =>
        Top[S](returnedType)

      case "clear timer" =>
        Top[S](returnedType)

      case "create sprite" =>
        Top[S](returnedType)

      case "current time" =>
        Top[S](returnedType)

      case "end" =>
        Top[S](returnedType)

      case "hud sprites" =>
        Top[S](returnedType)

      case "life" =>
        Top[S](returnedType)

      case "life visible" =>
        Top[S](returnedType)

      case "remove life" =>
        Top[S](returnedType)

      case "reset" =>
        Top[S](returnedType)

      case "score" =>
        Top[S](returnedType)

      case "set background scene" =>
        Top[S](returnedType)

      case "set hud colors" =>
        Top[S](returnedType)

      case "set life" =>
        Top[S](returnedType)

      case "set life visible" =>
        Top[S](returnedType)

      case "splash" =>
        Top[S](returnedType)

      case "splash text" =>
        Top[S](returnedType)

      case "sprites" =>
        Top[S](returnedType)

      case "start" =>
        Top[S](returnedType)

      case "start landscape" =>
        Top[S](returnedType)

      case "start portrait" =>
        Top[S](returnedType)

      case "start timer" =>
        Top[S](returnedType)

      case "start with background picture" =>
        Top[S](returnedType)

      case "start with fixed size" =>
        Top[S](returnedType)

      case _ =>
        super.forwardSemantics(this0,method,parameters,returnedType)
    }
  }
}