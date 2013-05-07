
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.Reporter
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

/**
 * Specifies the abstract semantics of Xml Object
 *
 * An xml element or collection of elements
 *
 * @author Lucas Brutschy
 */ 

object TXml_Object {

  /** Gets the list of attribute names */
  val field_attributes = new TouchField("  attributes",TString_Map.typ,NewInitializer())

  /** Indicates if this instance is an element or a filtered collection */
  val field_is_element = new TouchField("is element",TBoolean.typ,ExpressionInitializer(True(null)))

  /** Gets the concatenated text contents of this element */
  val field_value = new TouchField("value",TString.typ)

  /** Gets the namespace of this element */
  val field_namespace = new TouchField("namespace",TString.typ)

  /** Gets the local name of this element */
  val field_local_name = new TouchField("local name",TString.typ)

  val typName = "Xml Object"
  val typ = new TouchCollection(typName,TNumber.typName,TXml_Object.typName,List(field_attributes,field_is_element,field_local_name, field_namespace, field_value), immutableCollection = true)

}

class TXml_Object extends ACollection {

  def getTyp = TXml_Object.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the value of the attribute */
    case "attr" =>
      val List(name) = parameters // String
      CallApi[S](Field[S](this0,TXml_Object.field_attributes),"at",List(name),TString.typ)

    /** Gets the list of attribute names */
    case "attr names" =>
      CallApi[S](Field[S](this0,TXml_Object.field_attributes),"keys",Nil,TString_Collection.typ)

    /** Gets a first child element matching the fully qualified name */
    case "child" =>
      val List(name) = parameters // String
      if(TouchAnalysisParameters.reportDummyImplementations)
        Reporter.reportImprecision("Xml Object.child is a dummy",pp)
      TopWithInvalid[S](TXml_Object.typ,Map(
        TXml_Object.field_is_element -> False
      ))

    /** Gets a collection of child element matching the fully qualified name */
    case "children" =>
      val List(name) = parameters // String
      if(TouchAnalysisParameters.reportDummyImplementations)
        Reporter.reportImprecision("Xml Object.children is a dummy",pp)
      TopWithInvalid[S](TXml_Object.typ,Map(
        TXml_Object.field_is_element -> False
      ))

    /** Creates a qualified full name from the namespace and local name */
    case "create name" =>
      val List(local_name,namespace_uri) = parameters // String,String
      if(TouchAnalysisParameters.reportDummyImplementations)
        Reporter.reportImprecision("Xml Object.create name is a dummy",pp)
      Top[S](TString.typ)

    /** Gets the full name of this element */
    case "name" =>
      CallApi[S](this0,"create name",List(
        Field[S](this0,TXml_Object.field_local_name),
        Field[S](this0,TXml_Object.field_namespace)
      ),TNothing.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
