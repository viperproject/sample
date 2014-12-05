package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, TypeList, TouchType}
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

case class GObjectConstructor(objectTyp:AAny) extends AAny {

  def typeName = TypeName("Constructor", List(objectTyp.typeName))

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case "create" =>
      New[S](objectTyp)

    case "create collection" =>
      New[S](GObjectCollection(objectTyp))

    case "invalid" =>
      Return[S](Invalid(objectTyp,"invalid objected created"))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
