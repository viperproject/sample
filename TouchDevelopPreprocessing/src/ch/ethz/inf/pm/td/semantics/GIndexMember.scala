package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * @param typeName the name of the type
 * @param valueFields To access the value of our index member fields (the object with get/set/clear)
 *                    we pass the "access path" to that field. So for this->someField->value we pass (someField,value)
 */
case class GIndexMember(typeName: TypeName, keyFields: List[ApiField], valueFields: List[ApiField]) extends AAny {

  override def possibleFields = super.possibleFields ++ keyFields ++ valueFields

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    case "clear fields" =>
      var curState = state
      for (valueField <- valueFields) {
        curState = AssignField[S](this0, valueField, Invalid(valueField.typ, "fields may have been cleared"))(curState, pp)
      }
      curState

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

}
