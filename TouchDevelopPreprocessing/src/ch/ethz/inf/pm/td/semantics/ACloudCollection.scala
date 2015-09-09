package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.oorepresentation.Modifier
import ch.ethz.inf.pm.td.cloud.{CloudUpdateWrapper, CloudQueryWrapper}

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

}
