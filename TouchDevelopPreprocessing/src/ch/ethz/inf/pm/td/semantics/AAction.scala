package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * General definition for Actions (closure types)
 *
 * @author Lucas Brutschy
 */

object AAction {

  /** Stores a string representing the handler in the code. When an action is defined in the code, the
    * corresponding action is created with a unique name (e.g. program point based) and this object is
    * returned with the handlerName field set to the name of the created action. If this field
    * is top, and run is executed, we have to go to top, since we do not know what is executed */
  val field_handlerName = new TouchField("*handlername",TString.typName)

}


abstract class AAction extends AAny {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Run the inline action. */
    case "run" =>
      //RunActionFromString[S](Field[S](this0,AAction.field_handlerName),parameters)       TODO
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
