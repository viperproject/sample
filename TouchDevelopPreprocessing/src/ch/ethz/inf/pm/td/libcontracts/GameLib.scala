package ch.ethz.inf.pm.td.libcontracts

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichExpressionImplicits
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * @author Lucas Brutschy
 */
case object GameLib extends LibraryContract with RichExpressionImplicits {

  override val name = "game"

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet],
                                               returnedType: TouchType)(implicit pp: ProgramPoint, state: S) = {
    method match {

      case _ =>
        super.forwardSemantics(this0,method,parameters,returnedType)

    }
  }
}