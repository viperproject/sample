
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Editor
 *
 * An interface to TouchDevelop editor
 *
 * @author Lucas Brutschy
 */ 

object TEditor {

  val typName = "Editor"
  val typ = DefaultTouchType(typName)

}

class TEditor extends AAny {

  def getTyp = TEditor.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** [**beta**] Place a message on an AST node */
    // case "annotate ast" => 
    //   val List(id,category,message) = parameters // String,String,String
    //   Skip

    /** [**beta**] Returns the AST of the script currently in the editor */
    // case "current script ast" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TJson_Object.typ)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Returns the AST of the script currently in the editor */
    //   val field_current_script_ast = new TouchField("current script ast",TJson_Object.typName)

    /** [**beta**] The id of the script currently in the editor */
    // case "current script id" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] The id of the script currently in the editor */
    //   val field_current_script_id = new TouchField("current script id",TString.typName)

    /** [**beta**] Replace standard 'running plugin' message with something else */
    // case "progress" => 
    //   val List(message) = parameters // String
    //   Skip

    // FIELDS: field_current_script_ast, field_current_script_id

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
