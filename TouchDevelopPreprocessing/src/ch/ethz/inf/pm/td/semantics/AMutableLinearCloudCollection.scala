/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.cloud.{CloudUpdateWrapper, CloudQueryWrapper}

/**
 * A mutable collection that may be stored in the cloud
 *
 * @author Lucas Brutschy
 */
trait AMutableLinearCloudCollection extends AMutableLinearCollection with ALinearCloudCollection {

  override def member_add_many = super.member_add_many.copy(semantics = CloudUpdateWrapper(super.member_add_many.semantics,modifiers))
  override def member_add = super.member_add.copy(semantics = CloudUpdateWrapper(super.member_add.semantics,modifiers))
  override def member_clear = super.member_clear.copy(semantics = CloudUpdateWrapper(super.member_clear.semantics,modifiers))
  override def member_contains = super.member_contains.copy(semantics = CloudQueryWrapper(super.member_contains.semantics,modifiers))
  override def member_index_of = super.member_index_of.copy(semantics = CloudQueryWrapper(super.member_index_of.semantics,modifiers))
  override def member_insert_at = super.member_insert_at.copy(semantics = CloudUpdateWrapper(super.member_insert_at.semantics,modifiers))
  override def member_remove_at = super.member_remove_at.copy(semantics = CloudUpdateWrapper(super.member_remove_at.semantics,modifiers))
  override def member_remove = super.member_remove.copy(semantics = CloudUpdateWrapper(super.member_remove.semantics,modifiers))
  override def member_reverse = super.member_reverse.copy(semantics = CloudUpdateWrapper(super.member_reverse.semantics,modifiers))
  override def member_set_at = super.member_set_at.copy(semantics = CloudUpdateWrapper(super.member_set_at.semantics,modifiers))
  override def member_sort = super.member_sort.copy(semantics = CloudUpdateWrapper(super.member_sort.semantics,modifiers))

}
