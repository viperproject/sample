package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

case class GTable(rowTyp: AAny) extends AMutable_Collection {

  override def typeName: TypeName = TypeName("Table",List(rowTyp.typeName))
  override def keyType = TNumber
  override def valueType = rowTyp

  /** Backlink. Not a field of this table, but a field of all rows of the table */
  val field_table = new ApiField("*table", this)

  lazy val member_add_row = ApiMember(
    name = "add row",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated = true),
    returnType = rowTyp,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        // Create row with backlink to this table for removal
        var newState = New[S](rowTyp, initials = Map(field_table -> this0))(state, pp)
        val row = newState.expr
        newState = collectionInsert[S](this0, collectionSize[S](this0)(newState, pp), row)(newState, pp)
        newState = collectionIncreaseLength[S](this0)(newState, pp)
        Return[S](row)(newState, pp)
      }
    }
  )

  /** Just another name for at index */
  lazy val member_row_at = super.member_at_index

  lazy val member_invalid_row = ApiMember(
    name = "invalid row",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = rowTyp,
    semantics = InvalidSemantics
  )

  override lazy val declarations:Map[String,ApiMember] = super.declarations ++
    Map(
      "add row" -> member_add_row,
      "invalid row" -> member_invalid_row,
      "row at" -> member_row_at
    )


}
