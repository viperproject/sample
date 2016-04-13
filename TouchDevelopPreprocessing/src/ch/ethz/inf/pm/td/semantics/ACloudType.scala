/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.oorepresentation.Modifier
import ch.ethz.inf.pm.td.cloud.{CloudQueryWrapper, CloudUpdateWrapper}

/**
 * A type that may be stored in the cloud
 */
trait ACloudType extends AAny {

  def modifiers:Set[Modifier]

  override def member_:= = super.member_:=.copy(semantics = CloudUpdateWrapper(super.member_:=.semantics,modifiers))
  override def member_is_invalid = super.member_is_invalid.copy(semantics = CloudQueryWrapper(super.member_is_invalid.semantics,modifiers))
  override def member_∥ = super.member_∥.copy(semantics = CloudQueryWrapper(super.member_∥.semantics,modifiers))
  override def member_post_to_wall = super.member_post_to_wall.copy(semantics = CloudQueryWrapper(super.member_post_to_wall.semantics,modifiers))
  override def member_equals = super.member_equals.copy(semantics = CloudQueryWrapper(super.member_equals.semantics,modifiers))

}
