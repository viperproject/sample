package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

case class GRow(typeName: TypeName, fields: List[ApiField]) extends AAny {

  override def possibleFields = super.possibleFields ++ (GTable(this).field_table :: fields)

  lazy val member_delete_row = ApiMember(
    name = "delete row",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated = true),
    returnType = TBoolean,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        CallApi[S](Field[S](this0, GTable(GRow(typeName,fields)).field_table), "remove", List(this0), TBoolean)
      }
    }
  )

  lazy val member_confirmed = ApiMember(
    name = "confirmed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = ValidPureSemantics
  )


  override lazy val declarations:Map[String,ApiMember] = super.declarations ++ Map(
      "delete row" -> member_delete_row,
      "confirmed" -> member_confirmed
    ) ++ mkGetterSetters(GTable(this).field_table :: fields)


  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    case "confirmed" =>
      Top[S](TBoolean)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

}
