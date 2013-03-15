package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import RichNativeSemantics._

class AIndex(indexType:TouchType,keyTypes:List[TouchType],indexMemberType:TouchType) extends ACollection {

  def getTyp = indexType

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    // This overrides the default "at" behavior of collections: Instead of returning "invalid" for a new key, we
    // create a new value of that key. Since we do not really track keys, we create a new object every times
    case "at" =>
      val List(key) = parameters
      var curState = state
      curState = New[S](indexMemberType)(curState,pp)
      curState = CollectionInsert[S](this0,key,curState.getExpression())(curState,pp)
      Return[S](CollectionAt[S](this0,key)(curState,pp))(curState,pp)

    case "clear" =>
      CollectionClear[S](this0)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
