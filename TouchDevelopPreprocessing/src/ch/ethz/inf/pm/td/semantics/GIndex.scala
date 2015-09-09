package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{Modifier, ProgramPoint}
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.cloud.CloudQuerySemantics
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain.TouchStateInterface
import ch.ethz.inf.pm.td.parser.{Parameter, TypeName}

/**
 * Implements and Index with potentially multiple keys
 *
 * @author Lucas Brutschy
 */
case class GIndex(keyTypeParameters:List[TypeName] = List.empty, valueType:AAny, modifiers:Set[Modifier]) extends AIndex {

  def typeName = TypeName("Index",List(valueType.typeName))

  lazy val keyTypes:List[AAny] = TypeList.toTouchTypes(keyTypeParameters)

  lazy val tupleType = GTuple(keyTypes)

  override def keyType = if (keyTypes.size != 1) tupleType else keyTypes.head

  /** Sometimes used: Gets the picture at position 'index'; invalid if index is out of bounds */
  override lazy val member_at = if (keyTypes.size != 1) super.member_at.copy(
    semantics = new CloudQuerySemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {

        if (SystemParameters.DEBUG) assert(parameters.size == tupleType.sortedKeyFields.size)


        // Only distinguish elements if we have a single key
//        val key = if (parameters.size == 1) {
//          toRichExpression(parameters.head)
//        } else {
//          Valid(keyType)
//        }

        var curState = super.forwardSemantics[S](this0,method,parameters)

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

      override def typeModifiers: Set[Modifier] = modifiers
    }
  ) else super.member_at

}
