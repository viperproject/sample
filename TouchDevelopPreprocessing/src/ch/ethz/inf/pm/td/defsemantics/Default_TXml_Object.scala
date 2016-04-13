
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Xml Object
 *
 * An xml element or collection of elements
 *
 * @author Lucas Brutschy
 */

trait Default_TXml_Object extends ALinearCollection {

  lazy val typeName = TypeName("Xml Object")
          
  def keyType = TNumber

  def valueType = TXml_Object

  /** Never used: Gets the list of attribute names */
  def member_attr_names = ApiMember(
    name = "attr names",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the value of the attribute */
  def member_attr = ApiMember(
    name = "attr",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets a first child element matching the fully qualified name */
  def member_child = ApiMember(
    name = "child",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TXml_Object,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets a collection of child element matching the fully qualified name */
  def member_children = ApiMember(
    name = "children",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TXml_Object,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a qualified full name from the namespace and local name */
  def member_create_name = ApiMember(
    name = "create name",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Indicates if this instance is an element or a filtered collection */
  def member_is_element = ApiMember(
    name = "is element",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the local name of this element */
  def member_local_name = ApiMember(
    name = "local name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the full name of this element */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the namespace of this element */
  def member_namespace = ApiMember(
    name = "namespace",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets an xml string */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the concatenated text contents of this element */
  def member_value = ApiMember(
    name = "value",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "attr names" -> member_attr_names,
    "attr" -> member_attr,
    "child" -> member_child,
    "children" -> member_children,
    "create name" -> member_create_name,
    "is element" -> member_is_element,
    "local name" -> member_local_name,
    "name" -> member_name,
    "namespace" -> member_namespace,
    "to string" -> member_to_string,
    "value" -> member_value
  )
            

}
          
