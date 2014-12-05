package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, TouchType}
import ch.ethz.inf.pm.td.parser.TypeName

trait AIndex extends ALinearCollection {

  override def keyType = TNumber

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the i-th element */
    case "at index" =>
      val List(index) = parameters
      // Check disabled -- ALWAYS FALSE ALARM!
      // CheckInRangeInclusive(index, 0, CollectionSize[S](this0) - 1, "at index", "index")
      Return[S](collectionAllValues[S](this0))

    // This overrides the default "at" behavior of collections: Instead of returning "invalid" for a new key, we
    // create a new value of that key. Since we do not really track keys, we create a new object every times
    // TODO: This does not work for multi-key indexes
    case "at" =>
      val key = parameters.head
      If[S](collectionContainsKey[S](this0, key) equal False, Then=(state) => {
        var newState = New[S](valueType)(state,pp)
        val newIndexMember = newState.expr
        newState = collectionInsert[S](this0, key, newIndexMember)(newState,pp)
        newState = collectionIncreaseLength[S](this0)(newState, pp)
        Return[S](collectionAt[S](this0, key)(newState, pp))(newState, pp)
      }, Else=(state)=>{
        Return[S](collectionAt[S](this0, key)(state, pp))(state, pp)
      })

    case "clear" =>
      collectionClear[S](this0)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
