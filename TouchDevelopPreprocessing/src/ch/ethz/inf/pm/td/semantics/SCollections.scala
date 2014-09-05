package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of collections
 *
 * Create collections of items.
 *
 * @author Lucas Brutschy
 */

object SCollections extends ASingleton {

  lazy val typeName = TypeName("Collections")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates an empty user collection */
    case "create user collection" =>
      New[S](GCollection(TUser.typeName))

    /** Creates an empty picture collection */
    case "create picture collection" =>
      New[S](GCollection(TPicture.typeName))

    /** Creates an empty sound collection */
    case "create sound collection" =>
      New[S](GCollection(TSound.typeName))

    /** Creates an empty action collection */
    case "create action collection" =>
      New[S](GCollection(TAction.typeName))

    /** Creates an empty link collection */
    case "create link collection" =>
      New[S](TLink_Collection)

    /** Creates an empty location collection */
    case "create location collection" =>
      New[S](TLocation_Collection)

    /** Creates an empty message collection */
    case "create message collection" =>
      New[S](TMessage_Collection)

    /** Creates an empty number collection */
    case "create number collection" =>
      New[S](TNumber_Collection)

    /** Creates an empty number map */
    case "create number map" =>
      New[S](TNumber_Map)

    /** Creates an empty place collection */
    case "create place collection" =>
      New[S](TPlace_Collection)

    /** Creates an empty string collection */
    case "create string collection" =>
      New[S](TString_Collection)

    /** Creates an empty string map (case and culture sensitive) */
    case "create string map" =>
      New[S](TString_Map)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)


  }
}