
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichExpression._

/**
 * Specifies the abstract semantics of Pictures
 *
 * A collection of pictures
 *
 * @author Lucas Brutschy
 */ 

object TPictures {

  val typName = "Pictures"
  val typ = TouchCollection(typName,TNumber.typ,TPicture.typ)

}

class TPictures extends ACollection {

  def getTyp = TPictures.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Finds a picture by name and returns the index. Returns -1 if not found. */
    case "find" =>
      val List(name) = parameters // String
      Return[S](-1 ndTo CollectionSize[S](this0))

    /** Gets the thumbnail of i-th picture. */
    case "thumbnail" =>
      val List(index) = parameters // Number
      New[S](TPicture.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
