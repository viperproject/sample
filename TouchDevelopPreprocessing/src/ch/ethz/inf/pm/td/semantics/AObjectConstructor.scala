package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import RichNativeSemantics._

class AObjectConstructor(constructorTyp:TouchType,objectTyp:TouchType,collectionTyp:TouchType) extends AAny {

  def getTyp = constructorTyp

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case "create" =>
      New[S](objectTyp)

    case "create collection" =>
      New[S](collectionTyp)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
