
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichNativeSemantics, MethodSummaries}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * This implements helper functions that I use for the analysis
 *
 * @author Lucas Brutschy
 */

object SHelpers extends ASingleton {

  lazy val typeName = TypeName("Helpers",isSingleton = true)

  val CreateMethod = """create (.*action) (.+)""".r

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates an action with the given name */
    case CreateMethod(handlerTyp,handlerName) =>
      MethodSummaries.collectClosureEntry(handlerName,state)
      handlerTyp match {
        case "action" =>
          New[S](TAction,Map(TAction.field_handlerName -> String(handlerName)))
        case "boolean action" =>
          New[S](TBoolean_Action,Map(TBoolean_Action.field_handlerName -> String(handlerName)))
        case "number action" =>
          New[S](TNumber_Action,Map(TNumber_Action.field_handlerName -> String(handlerName)))
        case "position action" =>
          New[S](TPosition_Action,Map(TPosition_Action.field_handlerName -> String(handlerName)))
        case "text action" =>
          New[S](TText_Action,Map(TText_Action.field_handlerName -> String(handlerName)))
        case "sprite action" =>
          New[S](TSprite_Action,Map(TSprite_Action.field_handlerName -> String(handlerName)))
        case "sprite set action" =>
          New[S](TSprite_Set_Action,Map(TSprite_Set_Action.field_handlerName -> String(handlerName)))
        case "vector action" =>
          New[S](TVector_Action,Map(TVector_Action.field_handlerName -> String(handlerName)))
        case "web response action" =>
          New[S](TWeb_Response_Action,Map(TWeb_Response_Action.field_handlerName -> String(handlerName)))
        case "message collection action" =>
          New[S](TCollection_Message_Action,Map(TCollection_Message_Action.field_handlerName -> String(handlerName)))
        case _ =>
          super.forwardSemantics(this0,method,parameters,returnedType)
      }

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)


  }
}
      
