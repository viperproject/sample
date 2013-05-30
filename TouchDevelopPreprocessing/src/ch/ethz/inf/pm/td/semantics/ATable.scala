package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import RichNativeSemantics._

class ATable(tableTyp:TouchType,rowTyp:TouchType) extends ALinearCollection {

  def getTyp = tableTyp

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the i-th element */
    case "at index" =>
      val List(index) = parameters
      CheckInRangeInclusive(index, 0, CollectionSize[S](this0) - 1, "at index", "index")
      Return[S](CollectionSummary[S](this0))

    case "add row" =>
      var newState = New[S](rowTyp)(state, pp)
      val row = newState.getExpression()
      newState = CollectionInsert[S](this0, CollectionSize[S](this0)(newState,pp), row)(newState,pp)
      newState = CollectionIncreaseLength[S](this0)(newState, pp)
      Return[S](row)(newState, pp)

    case "clear" =>
      CollectionClear[S](this0)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
