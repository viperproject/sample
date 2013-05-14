package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{SemanticException, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{TouchType, TouchCollection}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalAnalysisConstants

/**
 * Represents a collection (this class contains common read operations. Extend AMutable_Collections to get write ops)
 */
abstract class ACollection extends AAny {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a copy of the given collection. AUXILIARY FUNCTION FOR FOREACH LOOPS */
    case "copy" =>
      Clone[S](this0)

    //TODO: why is this here?
    /** Get random element */
    case "rand" =>
      If[S]( CollectionSize[S](this0) > 0, Then = {
        Return[S](CollectionSummary[S](this0))(_,pp)
      }, Else = {
        Return[S](Invalid(this0.getType()))(_,pp)
      })

    /** Returns the length of the collection*/
    case "count" =>
      Return[S](CollectionSize[S](this0))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}