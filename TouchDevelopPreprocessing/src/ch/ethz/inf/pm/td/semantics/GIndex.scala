package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{TypeList, ApiMember, ApiMemberSemantics}
import ch.ethz.inf.pm.td.parser.{Parameter, TypeName}

/**
 * Implements and Index with potentially multiple keys
 *
 * @author Lucas Brutschy
 */
case class GIndex(keyTypeParameters:List[TypeName] = List.empty, valueType:AAny) extends AIndex {

  def typeName = TypeName("Index",List(valueType.typeName))

  lazy val keyTypes:List[AAny] = TypeList.toTouchTypes(keyTypeParameters)

  lazy val tupleType = GTuple(keyTypes)

  override def keyType = if (keyTypes.size != 1) tupleType else keyTypes.head

  /** Sometimes used: Gets the picture at position 'index'; invalid if index is out of bounds */
  override lazy val member_at = if (keyTypes.size != 1) super.member_at.copy(
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {

        if (SystemParameters.DEBUG) assert(parameters.size == tupleType.sortedKeyFields.size)

        // Only distinguish elements if we have a single key
//        val key = if (parameters.size == 1) {
//          toRichExpression(parameters.head)
//        } else {
//          Valid(keyType)
//        }

        var curState = state
        curState = New[S](keyType, tupleType.sortedKeyFields.zip(parameters.map(toRichExpression)).toMap)(curState,pp)
        val key = curState.expr
        If[S](ContainsKey[S](this0, key) equal False, Then=(state) => {
          var newState = New[S](valueType)(state,pp)
          val newIndexMember = newState.expr
          newState = Insert[S](this0, key, newIndexMember)(newState,pp)
          newState = IncreaseLength[S](this0)(newState, pp)
          Return[S](At[S](this0, key)(newState, pp))(newState, pp)
        }, Else=(state)=>{
          Return[S](At[S](this0, key)(state, pp))(state, pp)
        })(curState,pp)
      }

    }
  ) else super.member_at

}
