
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{TouchException, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.MethodSummaries

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

  val CreateMethod = """create (.*action) (.+)""".r
  def getTyp = SHelpers.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates an action with the given name */
    case CreateMethod(handlerTyp,handlerName) =>
      MethodSummaries.collectClosureEntry(handlerName,state)
      handlerTyp match {
        case "action" =>
          New[S](TAction.typ,Map(AAction.field_handlerName -> String(handlerName)))
        case "position action" =>
          New[S](TPosition_Action.typ,Map(AAction.field_handlerName -> String(handlerName)))
        case "text action" =>
          New[S](TText_Action.typ,Map(AAction.field_handlerName -> String(handlerName)))
        case "sprite action" =>
          New[S](TSprite_Action.typ,Map(AAction.field_handlerName -> String(handlerName)))
        case "sprite set action" =>
          New[S](TSprite_Set_Action.typ,Map(AAction.field_handlerName -> String(handlerName)))
        case "vector action" =>
          New[S](TVector_Action.typ,Map(AAction.field_handlerName -> String(handlerName)))
        case "web response action" =>
          New[S](TWeb_Response_Action.typ,Map(AAction.field_handlerName -> String(handlerName)))
        case "message collection action" =>
          New[S](TMessage_Collection_Action.typ,Map(AAction.field_handlerName -> String(handlerName)))
        case _ =>
          super.forwardSemantics(this0,method,parameters,returnedType)
      }

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)


  }
}
      
