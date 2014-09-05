
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Web Event Source
 *
 * A Server-Sent-Events client
 *
 * @author Lucas Brutschy
 */ 

object TWeb_Event_Source extends AAny {

  val typeName = TypeName("Web Event Source")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Closes the EventSource. No further event will be raised. */
    // case "close" =>
    //   val List() = parameters // 
    //   Skip

    /** Sets an event to run when an error occurs */
    // case "on error" => 
    //   val List(handler) = parameters // {"g":"Action","a":[]}
    //   TopWithInvalid[S](TEvent_Binding)
    // DECLARATION AS FIELD: 
    //   /** Sets an event to run when an error occurs */
    //   lazy val field_on_error = new TouchField("on error",TEvent_Binding.typeName)

    /** Sets an event to run when a message is received. Change name to receive custom events. */
    // case "on message" => 
    //   val List(name,handler) = parameters // String,{"g":"Text_Action","a":[]}
    //   TopWithInvalid[S](TEvent_Binding)
    // DECLARATION AS FIELD: 
    //   /** Sets an event to run when a message is received. Change name to receive custom events. */
    //   lazy val field_on_message = new TouchField("on message",TEvent_Binding.typeName)

    /** Sets an event to run when the event source is opened */
    // case "on open" => 
    //   val List(opened) = parameters // {"g":"Action","a":[]}
    //   TopWithInvalid[S](TEvent_Binding)
    // DECLARATION AS FIELD: 
    //   /** Sets an event to run when the event source is opened */
    //   lazy val field_on_open = new TouchField("on open",TEvent_Binding.typeName)

    /** Gets the current connection state (`connecting`, `open`, `closed`) */
    // case "state" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString)
    // DECLARATION AS FIELD: 
    //   /** Gets the current connection state (`connecting`, `open`, `closed`) */
    //   lazy val field_state = new TouchField("state",TString.typeName)

    // FIELDS: field_on_error, field_on_message, field_on_open, field_state

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
