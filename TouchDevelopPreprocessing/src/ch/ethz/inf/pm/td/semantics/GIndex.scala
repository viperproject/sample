package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiMemberSemantics}
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Implements and Index with potentially multiple keys
 *
 * @author Lucas Brutschy
 */
case class GIndex(keyTypes:List[AAny], valueType:AAny) extends AIndex {

  def typeName = TypeName("Index",List(valueType.typeName))

  val tupleType = GTuple(keyTypes)

  override def keyType = if (keyTypes.size != 1) tupleType else keyTypes.head

  /** Sometimes used: Gets the picture at position 'index'; invalid if index is out of bounds */
  override lazy val member_at = if (keyTypes.size != 1) super.member_at.copy(
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {

        assert(parameters.size == tupleType.sortedKeyFields.size)

        // Create a tuple representing the key that is accessed
        var curState = state
        curState = New[S](keyType, tupleType.sortedKeyFields.zip(parameters.map(toRichExpression)).toMap)(curState,pp)
        val key = curState.expr
        If[S](collectionContainsKey[S](this0, key) equal False, Then=(state) => {
          var newState = New[S](valueType)(state,pp)
          val newIndexMember = newState.expr
          newState = collectionInsert[S](this0, key, newIndexMember)(newState,pp)
          newState = collectionIncreaseLength[S](this0)(newState, pp)
          Return[S](collectionAt[S](this0, key)(newState, pp))(newState, pp)
        }, Else=(state)=>{
          Return[S](collectionAt[S](this0, key)(state, pp))(state, pp)
        })(curState,pp)
      }

    }
  ) else super.member_at

}
