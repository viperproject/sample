
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Xml Object
 *
 * An xml element or collection of elements
 *
 * @author Lucas Brutschy
 */ 

object TXml_Object {

  val typName = "Xml_Object"
  val typ = new TouchType(typName,isSingleton = false,List())

}

class TXml_Object extends AAny {

  def getTyp = TXml_Object.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the i-th child element in the collection */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TXml_Object.typ))

    /** Gets the value of the attribute */
    // case "attr" => 
    //   val List(name) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Gets the list of attribute names */
    // case "attr_names" => 
    //   Return[S](Valid(TString_Collection.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the list of attribute names */
    //   val field_attr_names = new TouchField("attr_names",TString_Collection.typ)

    /** Gets a first child element matching the fully qualified name */
    // case "child" => 
    //   val List(name) = parameters // String
    //   Return[S](Valid(TXml_Object.typ))

    /** Gets a collection of child element matching the fully qualified name */
    // case "children" => 
    //   val List(name) = parameters // String
    //   Return[S](Valid(TXml_Object.typ))

    /** Gets the number of child element */
    // case "count" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the number of child element */
    //   val field_count = new TouchField("count",TNumber.typ)

    /** Creates a qualified full name from the namespace and local name */
    // case "create_name" => 
    //   val List(local_name,namespace_uri) = parameters // String,String
    //   Return[S](Valid(TString.typ))

    /** Indicates if this instance is an element or a filtered collection */
    // case "is_element" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if this instance is an element or a filtered collection */
    //   val field_is_element = new TouchField("is_element",TBoolean.typ)

    /** Gets the local name of this element */
    // case "local_name" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the local name of this element */
    //   val field_local_name = new TouchField("local_name",TString.typ)

    /** Gets the full name of this element */
    // case "name" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the full name of this element */
    //   val field_name = new TouchField("name",TString.typ)

    /** Gets the namespace of this element */
    // case "namespace" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the namespace of this element */
    //   val field_namespace = new TouchField("namespace",TString.typ)

    /** Gets an xml string */
    // case "to_string" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets an xml string */
    //   val field_to_string = new TouchField("to_string",TString.typ)

    /** Gets the concatenated text contents of this element */
    // case "value" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the concatenated text contents of this element */
    //   val field_value = new TouchField("value",TString.typ)

    // FIELDS: , field_attr_names, field_count, field_is_element, field_local_name, field_name, field_namespace, field_to_string, field_value

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
