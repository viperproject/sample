
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ExpressionInitializer, NewInitializer, ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Xml Object
 *
 * An xml element or collection of elements
 *
 * @author Lucas Brutschy
 */

object TXml_Object extends ALinearCollection {

  /** Gets the list of attribute names */
  lazy val field_attributes = new ApiField("  attributes", TString_Map.typeName, NewInitializer)

  /** Indicates if this instance is an element or a filtered collection */
  lazy val field_is_element = new ApiField("is element", TBoolean.typeName, ExpressionInitializer(True(null)))

  /** Gets the concatenated text contents of this element */
  lazy val field_value = new ApiField("value", TString.typeName)

  /** Gets the namespace of this element */
  lazy val field_namespace = new ApiField("namespace", TString.typeName)

  /** Gets the local name of this element */
  lazy val field_local_name = new ApiField("local name", TString.typeName)

  val typeName = TypeName("Xml Object")

  def keyTypeName = TNumber.typeName

  def valueTypeName = TXml_Object.typeName

  override def possibleFields = super.possibleFields ++ List(field_attributes,
    field_is_element, field_local_name, field_namespace, field_value)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Gets the value of the attribute */
    case "attr" =>
      val List(name) = parameters // String
      CallApi[S](Field[S](this0, TXml_Object.field_attributes), "at", List(name), TString)

    /** Gets the list of attribute names */
    case "attr names" =>
      CallApi[S](Field[S](this0, TXml_Object.field_attributes), "keys", Nil, TString_Collection)

    /** Gets a first child element matching the fully qualified name */
    case "child" =>
      val List(name) = parameters // String
      Dummy[S](this0, method)
      TopWithInvalid[S](TXml_Object, "corresponding child may not exist", Map(
        TXml_Object.field_is_element -> False
      ))

    /** Gets a collection of child element matching the fully qualified name */
    case "children" =>
      val List(name) = parameters // String
      Dummy[S](this0, method)
      TopWithInvalid[S](TXml_Object, "corresponding children may not exist", Map(
        TXml_Object.field_is_element -> False
      ))

    /** Creates a qualified full name from the namespace and local name */
    case "create name" =>
      val List(local_name, namespace_uri) = parameters // String,String
      Dummy[S](this0, method)
      Top[S](TString)

    /** Gets the full name of this element */
    case "name" =>
      CallApi[S](this0, "create name", List(
        Field[S](this0, TXml_Object.field_local_name),
        Field[S](this0, TXml_Object.field_namespace)
      ), TNothing)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
