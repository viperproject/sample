
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Web Event Source
 *
 * A Server-Sent-Events client
 *
 * @author Lucas Brutschy
 */ 

object TWeb_Event_Source {

  val typName = "Web Event Source"
  val typ = DefaultTouchType(typName)

}

class TWeb_Event_Source extends AAny {

  def getTyp = TWeb_Event_Source.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Closes the EventSource. No further event will be raised. */
    // case "close" =>
    //   val List() = parameters // 
    //   Skip

    /** Sets an event to run when an error occurs */
    // case "on error" => 
    //   val List(handler) = parameters // {"g":"Action","a":[]}
    //   TopWithInvalid[S](TEvent_Binding.typ)
    // DECLARATION AS FIELD: 
    //   /** Sets an event to run when an error occurs */
    //   val field_on_error = new TouchField("on error",TEvent_Binding.typName)

    /** Sets an event to run when a message is received. Change name to receive custom events. */
    // case "on message" => 
    //   val List(name,handler) = parameters // String,{"g":"Text_Action","a":[]}
    //   TopWithInvalid[S](TEvent_Binding.typ)
    // DECLARATION AS FIELD: 
    //   /** Sets an event to run when a message is received. Change name to receive custom events. */
    //   val field_on_message = new TouchField("on message",TEvent_Binding.typName)

    /** Sets an event to run when the event source is opened */
    // case "on open" => 
    //   val List(opened) = parameters // {"g":"Action","a":[]}
    //   TopWithInvalid[S](TEvent_Binding.typ)
    // DECLARATION AS FIELD: 
    //   /** Sets an event to run when the event source is opened */
    //   val field_on_open = new TouchField("on open",TEvent_Binding.typName)

    /** Gets the current connection state (`connecting`, `open`, `closed`) */
    // case "state" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the current connection state (`connecting`, `open`, `closed`) */
    //   val field_state = new TouchField("state",TString.typName)

    // FIELDS: field_on_error, field_on_message, field_on_open, field_state

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
