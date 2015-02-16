package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SCollections
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of collections
 *
 * Create collections of items.
 *
 * @author Lucas Brutschy
 */

object SCollections extends Default_SCollections {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates an empty user collection */
    case "create user collection" =>
      New[S](GCollection(TUser))

    /** Creates an empty picture collection */
    case "create picture collection" =>
      New[S](GCollection(TPicture))

    /** Creates an empty sound collection */
    case "create sound collection" =>
      New[S](GCollection(TSound))

    /** Creates an empty action collection */
    case "create action collection" =>
      New[S](GCollection(TAction))

    /** Creates an empty link collection */
    case "create link collection" =>
      New[S](GCollection(TLink))

    /** Creates an empty location collection */
    case "create location collection" =>
      New[S](GCollection(TLocation))

    /** Creates an empty message collection */
    case "create message collection" =>
      New[S](GCollection(TMessage))

    /** Creates an empty number collection */
    case "create number collection" =>
      New[S](GCollection(TNumber))

    /** Creates an empty number map */
    case "create number map" =>
      New[S](TNumber_Map)

    /** Creates an empty place collection */
    case "create place collection" =>
      New[S](GCollection(TPlace))

    /** Creates an empty string collection */
    case "create string collection" =>
      New[S](GCollection(TString))

    /** Creates an empty string map (case and culture sensitive) */
    case "create string map" =>
      New[S](TString_Map)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)


  }
}