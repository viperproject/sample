
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Tiles
 *
 * tiles and notifications for Windows and Windows Phone
 *
 * @author Lucas Brutschy
 */ 

object STiles extends ASingleton {

  lazy val typeName = TypeName("Tiles")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Pins or updates the default tile. */
    case "pin default" =>
       val List() = parameters //
       Skip

    /** Pins or updates the default tile with a custom picture. */
    case "pin picture" =>
       val List(title,content,counter,pic) = parameters // String,String,Number,Picture
       Skip

    /** Pins or updates the default tile with custom pictures. */
    case "pin pictures" =>
       val List(title,counter,icon,pictures) = parameters // String,Number,Picture,{"g":"Collection","a":["Picture"]}
       Skip

    /** Sets the counter of the default tile. Hidden if the number is not between 1 or 99. */
    case "set default counter" =>
       val List(value) = parameters // Number
       Skip

    /** Sets the front of a standard tile. */
    case "set default text" =>
       val List(title,content) = parameters // String,String
       Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
