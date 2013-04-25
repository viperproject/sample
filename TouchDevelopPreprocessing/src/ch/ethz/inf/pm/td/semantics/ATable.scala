package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import RichNativeSemantics._

class ATable(tableTyp:TouchType,rowTyp:TouchType) extends ACollection {

  def getTyp = tableTyp

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case "add row" =>
      val state1 = New[S](rowTyp)
      val row = state1.getExpression()
      val state2 = CollectionInsert[S](this0,CollectionSize[S](this0)(state1,pp)-1,row)(state1,pp)
      Return[S](row)(state2,pp)

    case "clear" =>
      CollectionClear[S](this0)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
