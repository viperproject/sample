
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.parser.{TypeName, Parameter}

/**
 * This implements helper functions that I use for the analysis
 *
 * @author Lucas Brutschy
 */

object SHelpers {

  val typName = "Helpers"
  val typ = new TouchType(typName,isSingleton = true)

}

class SHelpers extends AAny {

  def getTyp = SHelpers.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates an action with the given name */
    case "create action" =>
      val List(name) = parameters // String
      New[S](TAction.typ,Map(AAction.field_handlerName -> name))

    /** Creates an action with the given name */
    case "create position action" =>
      val List(name) = parameters // String
      New[S](TPosition_Action.typ,Map(AAction.field_handlerName -> name))

    /** Creates an action with the given name */
    case "create text action" =>
      val List(name) = parameters // String
      New[S](TText_Action.typ,Map(AAction.field_handlerName -> name))

    /** Creates an action with the given name */
    case "create sprite action" =>
      val List(name) = parameters // String
      New[S](TSprite_Action.typ,Map(AAction.field_handlerName -> name))

    /** Creates an action with the given name */
    case "create sprite set action" =>
      val List(name) = parameters // String
      New[S](TSprite_Set_Action.typ,Map(AAction.field_handlerName -> name))

    /** Creates an action with the given name */
    case "create vector action" =>
      val List(name) = parameters // String
      New[S](TVector_Action.typ,Map(AAction.field_handlerName -> name))

    /** Creates an action with the given name */
    case "create web response action" =>
      val List(name) = parameters // String
      New[S](TWeb_Response_Action.typ,Map(AAction.field_handlerName -> name))

    /** Creates an action with the given name */
    case "create message collection action" =>
      val List(name) = parameters // String
      New[S](TMessage_Collection_Action.typ,Map(AAction.field_handlerName -> name))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
