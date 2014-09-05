package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, Identifier, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, TypeList, TouchType}
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Represents a collection (this class contains common read operations. Extend AMutable_Collections to get write ops)
 */
trait ACollection extends AAny {

  def keyTypeName:TypeName
  def valueTypeName:TypeName

  def keyType =   SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(keyTypeName)
  def valueType = SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(valueTypeName)

  lazy val field_count = TouchField("count",TNumber.typeName)
  lazy val field_entry = TouchField("entries",TypeName("Entry",List(keyTypeName,valueTypeName)))

  override def isSingleton: Boolean = false

  override def possibleFields: Set[Identifier] =
    super.possibleFields ++ Set(field_entry,field_count)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a copy of the given collection. AUXILIARY FUNCTION FOR FOREACH LOOPS */
    case "copy" =>
      Clone[S](this0)

    /** [**dbg**] Exports a JSON representation of the contents. */
    case "to json" =>
      Top[S](TJson_Object)

    /** [**dbg**] Imports a JSON representation of the contents. */
    case "from json" =>
      val List(jobj) = parameters // Json_Object UNSOUND
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
