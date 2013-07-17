package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import RichNativeSemantics._

class ARow(rowTyp:TouchType,tableField:TouchField) extends AAny {

  def getTyp = rowTyp

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case "delete row" =>
      CallApi[S](Field[S](this0,tableField),"remove",List(this0),TBoolean.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
