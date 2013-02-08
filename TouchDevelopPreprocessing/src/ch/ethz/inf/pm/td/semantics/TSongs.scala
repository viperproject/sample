
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Songs
 *
 * A collection of songs
 *
 * @author Lucas Brutschy
 */ 

object TSongs {

  val typName = "Songs"
  val typ = TouchCollection(typName,"Number","Song")

}

class TSongs extends ACollection {

  def getTyp = TSongs.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the song. */
    case "play" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
