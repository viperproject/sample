
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Form Builder
 *
 * A builder to create HTML Form data
 *
 * This is mostly a dummy
 *
 * @author Lucas Brutschy
 */ 

object TForm_Builder {

  val typName = "Form Builder"
  val typ = DefaultTouchType(typName,isSingleton = false)

}

class TForm_Builder extends AAny {

  def getTyp = TForm_Builder.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Adds a string value */
    case "add string" =>
      val List(name,value) = parameters // String,String
      Dummy[S](this0,method)
      Skip

    /** Adds a number value */
    case "add number" =>
      val List(name,value) = parameters // String,Number
      Dummy[S](this0,method)
      Skip

    /** Adds a boolean value */
    case "add boolean" =>
      val List(name,value) = parameters // String,Boolean
      Dummy[S](this0,method)
      Skip

    /** Adds a picture */
    case "add picture" =>
      val List(name,value,picture_Name) = parameters // String,Picture,String
      Dummy[S](this0,method)
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
