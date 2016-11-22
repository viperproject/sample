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
 * Specifies the abstract semantics of Dom
 *
 * Current html element in the page.
 *
 * @author Lucas Brutschy
 */

trait Default_SDom extends ASingleton {

  lazy val typeName = TypeName("Dom", isSingleton = true)
          
  /** Never used: [**beta**] Add a CSS class name to the current element. */
  def member_add_css_class = ApiMember(
    name = "add css class",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Set what happens when this element is clicked. */
  def member_add_on_click = ApiMember(
    name = "add on click",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Bind editable text, using a string reference. */
  def member_bind_value_to_ref = ApiMember(
    name = "bind value to ref",
    paramTypes = List(ApiParam(GRef(TString))),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Bind editable text, by giving current text and change handler. */
  def member_bind_value_with_handler = ApiMember(
    name = "bind value with handler",
    paramTypes = List(ApiParam(TString), ApiParam(TText_Action)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Specify an attribute for the current element. */
  def member_set_attribute = ApiMember(
    name = "set attribute",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Specify a style attribute for the current element. */
  def member_set_style = ApiMember(
    name = "set style",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Specify the tagname for this element */
  def member_set_tag_name = ApiMember(
    name = "set tag name",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Use CSS for layout and import additional CSS stylesheets. Use string art resource to import urls. */
  def member_use_css = ApiMember(
    name = "use css",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add css class" -> member_add_css_class,
    "add on click" -> member_add_on_click,
    "bind value to ref" -> member_bind_value_to_ref,
    "bind value with handler" -> member_bind_value_with_handler,
    "set attribute" -> member_set_attribute,
    "set style" -> member_set_style,
    "set tag name" -> member_set_tag_name,
    "use css" -> member_use_css
  )
            

}
          
