package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * Specifies the abstract semantics of collections
 *
 * Create collections of items.
 *
 * @author Lucas Brutschy
 */

object SCollections {

  val typName = "Collections"
  val typ = new TouchType(typName, isSingleton = true)

}

class SCollections extends AAny {

  def getTyp = SCollections.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates an empty user collection */
    case "create user collection" =>
      New[S](GCollection.typ(TUser.typName))

    /** Creates an empty picture collection */
    case "create picture collection" =>
      New[S](GCollection.typ(TPicture.typName))

    /** Creates an empty sound collection */
    case "create sound collection" =>
      New[S](GCollection.typ(TSound.typName))

    /** Creates an empty action collection */
    case "create action collection" =>
      New[S](GCollection.typ(TAction.typName))

    /** Creates an empty link collection */
    case "create link collection" =>
      New[S](TLink_Collection.typ)

    /** Creates an empty location collection */
    case "create location collection" =>
      New[S](TLocation_Collection.typ)

    /** Creates an empty message collection */
    case "create message collection" =>
      New[S](TMessage_Collection.typ)

    /** Creates an empty number collection */
    case "create number collection" =>
      New[S](TNumber_Collection.typ)

    /** Creates an empty number map */
    case "create number map" =>
      New[S](TNumber_Map.typ)

    /** Creates an empty place collection */
    case "create place collection" =>
      New[S](TPlace_Collection.typ)

    /** Creates an empty string collection */
    case "create string collection" =>
      New[S](TString_Collection.typ)

    /** Creates an empty string map (case and culture sensitive) */
    case "create string map" =>
      New[S](TString_Map.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)


  }
}