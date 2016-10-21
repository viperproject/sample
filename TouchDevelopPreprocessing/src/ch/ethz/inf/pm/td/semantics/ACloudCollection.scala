/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint}
import ch.ethz.inf.pm.td.cloud.{CloudQueryWrapper, CloudUpdateWrapper}
import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiMemberSemantics, ApiParam}

/**
 *
 * Collections that may be stored in the cloud
 *
 * @author Lucas Brutschy
 */
trait ACloudCollection extends ACollection with ACloudType {

  override def member_to_json = super.member_to_json.copy(semantics = CloudQueryWrapper(super.member_to_json.semantics,modifiers))
  override def member_count = super.member_count.copy(semantics = CloudQueryWrapper(super.member_count.semantics,modifiers))
  override def member_from_json = super.member_from_json.copy(semantics = CloudUpdateWrapper(super.member_from_json.semantics,modifiers))
  override def member_copy = super.member_copy.copy(semantics = CloudQueryWrapper(super.member_copy.semantics,modifiers))
  override def member_at_index = super.member_at_index.copy(semantics = CloudQueryWrapper(super.member_at_index.semantics,modifiers))
  override def declarations: Map[String, ApiMember] = super.declarations + (member__clear.name -> member__clear)

}
