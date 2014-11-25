package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

case class GRow(typeName: TypeName, fields: List[ApiField]) extends AAny {

  override def possibleFields = super.possibleFields ++ (GTable(typeName).field_table :: fields)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    case "delete row" =>
      CallApi[S](Field[S](this0, GTable(typeName).field_table), "remove", List(this0), TBoolean)

    case "confirmed" =>
      Top[S](TBoolean)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

}
