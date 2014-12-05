package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, TypeList, TouchType}
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._
import ch.ethz.inf.pm.td.semantics.TBuffer._

case class GTable(rowTyp: AAny) extends AMutable_Collection {

  /** Backlink. Not a field of this table, but a field of all rows of the table */
  val field_table = new ApiField("*table", this)

  override def typeName: TypeName = TypeName("Table",List(rowTyp.typeName))
  override def keyType = TNumber
  override def valueType = rowTyp

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    case "add row" =>
      // Create row with backlink to this table for removal
      var newState = New[S](rowTyp, initials = Map(field_table -> this0))(state, pp)
      val row = newState.expr
      newState = collectionInsert[S](this0, collectionSize[S](this0)(newState, pp), row)(newState, pp)
      newState = collectionIncreaseLength[S](this0)(newState, pp)
      Return[S](row)(newState, pp)

    case "row at" =>
      super.forwardSemantics(this0, "at index", parameters, returnedType)

    case "invalid row" =>
      Return[S](Invalid(rowTyp, "value may have been initialized to invalid"))

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
