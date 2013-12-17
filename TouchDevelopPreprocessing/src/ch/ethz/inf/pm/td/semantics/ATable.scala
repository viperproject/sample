package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import RichNativeSemantics._

class ATable(tableTyp:TouchType,rowTyp:TouchType,rowTableField:TouchField) extends AMutable_Collection {

  def getTyp = tableTyp

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case "add row" =>
      // Create row with backlink to this table for removal
      var newState = New[S](rowTyp, initials = Map(rowTableField -> this0))(state, pp)
      val row = newState.getExpression
      newState = CollectionInsert[S](this0, CollectionSize[S](this0)(newState,pp), row)(newState,pp)
      newState = CollectionIncreaseLength[S](this0)(newState, pp)
      Return[S](row)(newState, pp)

    case "row at" =>
      super.forwardSemantics(this0,"at index",parameters,returnedType)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
