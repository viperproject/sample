package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.cloud.CloudQueryWrapper

/**
 * A linear collection that may be stored in the cloud
 *
 * @author Lucas Brutschy
 */
trait ALinearCloudCollection extends ALinearCollection with ACloudCollection {

  override def member_at = super.member_at.copy(semantics = CloudQueryWrapper(super.member_at.semantics,modifiers))
  override def member_rand = super.member_rand.copy(semantics = CloudQueryWrapper(super.member_rand.semantics,modifiers))
  override def member_random = super.member_random.copy(semantics = CloudQueryWrapper(super.member_random.semantics,modifiers))

}
