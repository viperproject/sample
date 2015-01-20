package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint}
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName

trait AIndex extends ACollection {

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "clear" -> member_clear,
    "at" -> member_at
  )

  override def member_at_index = super.member_at_index.copy(semantics = new ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      Return[S](collectionAllValues[S](this0))
    }
  })

  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        collectionClear[S](this0)
      }
    }
  )

  /** Sometimes used: Gets the picture at position 'index'; invalid if index is out of bounds */
  def member_at = ApiMember(
    name = "at",
    paramTypes = List(ApiParam(keyType)),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val key = parameters.head
        If[S](collectionContainsKey[S](this0, key) equal False, Then=(state) => {
          var newState = New[S](valueType)(state,pp)
          val newIndexMember = newState.expr
          newState = collectionInsert[S](this0, key, newIndexMember)(newState,pp)
          newState = collectionIncreaseLength[S](this0)(newState, pp)
          Return[S](collectionAt[S](this0, key)(newState, pp))(newState, pp)
        }, Else=(state)=>{
          Return[S](collectionAt[S](this0, key)(state, pp))(state, pp)
        })(state,pp)
      }

    }
  )

}
