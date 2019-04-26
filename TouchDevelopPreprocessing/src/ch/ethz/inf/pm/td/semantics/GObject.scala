/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.oorepresentation.Modifier
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.{Parameter, TypeName}

case class GObject(typeName:TypeName, fieldsParameters:List[Parameter], modifiers:Set[Modifier]) extends AAny {

  lazy val fields:Set[ApiField] = TypeList.toTouchFields(fieldsParameters)

  lazy val member_clear_fields = ApiMember(
    name = "clear fields",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated = true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  lazy val member_to_json = ApiMember(
    name = "to json",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  lazy val member_from_json = ApiMember(
    name = "from json",
    paramTypes = List(ApiParam(TJson_Object)),
    thisType = ApiParam(this,isMutated = true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  override def possibleFields = super.possibleFields ++ fields

  override lazy val declarations = super.declarations ++ mkGetterSetters(fields) ++ Map(
    "clear fields" -> member_clear_fields,
    "to json" -> member_to_json,
    "from json" -> member_from_json
  )

}
