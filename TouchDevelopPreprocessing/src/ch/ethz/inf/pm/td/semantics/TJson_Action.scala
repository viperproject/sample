
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TJson_Action
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Json Action
 *
 * A place to hook up an action to execute in response to an event
 *
 * @author Lucas Brutschy
 */ 

object TJson_Action extends Default_TJson_Action {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Run the inline action. */
    // case "run" =>
    //   val List(json) = parameters // Json_Object
    //   Skip

    // FIELDS: 

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
