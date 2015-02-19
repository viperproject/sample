package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * @param typeName the name of the type
 * @param valueFields To access the value of our index member fields (the object with get/set/clear)
 *                    we pass the "access path" to that field. So for this->someField->value we pass (someField,value)
 */
case class GIndexMember(typeName: TypeName, keyFields: List[ApiField], valueFields: List[ApiField]) extends AAny {

  def member_clear_fields = ApiMember(
    name = "clear fields",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated = true),
    returnType = TNothing,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        var curState = state
        for (valueField <- valueFields) {
          curState = AssignField[S](this0, valueField, Invalid(valueField.typ, "fields may have been cleared"))(curState, pp)
        }
        curState
      }
    }
  )

  def member_is_deleted = ApiMember(
    name = "is deleted",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated = true),
    returnType = TBoolean,
    semantics = ValidPureSemantics
  )

  override def possibleFields =
    super.possibleFields ++ keyFields ++ valueFields

  override lazy val declarations =
    super.declarations ++ mkGetterSetters(keyFields ::: valueFields) +
      ("clear fields" -> member_clear_fields, "is deleted" -> member_is_deleted)

}
