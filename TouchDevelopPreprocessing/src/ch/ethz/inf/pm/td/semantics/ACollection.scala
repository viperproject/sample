package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._

object ACollection {

  val field_count = new TouchField("count", TNumber.typName)

}

/**
 * Represents a collection (this class contains common read operations. Extend AMutable_Collections to get write ops)
 */
abstract class ACollection extends AAny {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a copy of the given collection. AUXILIARY FUNCTION FOR FOREACH LOOPS */
    case "copy" =>
      Clone[S](this0)

    /** Returns the length of the collection*/
    case "count" =>
      Return[S](CollectionSize[S](this0))

    /** [**dbg**] Exports a JSON representation of the contents. */
    case "to json" =>
      Top[S](TJson_Object.typ)

    /** [**dbg**] Imports a JSON representation of the contents. */
    case "from json" =>
      val List(jobj) = parameters // Json_Object UNSOUND
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
