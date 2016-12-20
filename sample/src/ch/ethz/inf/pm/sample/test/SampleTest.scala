/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.DummyTypeMap
import org.scalatest.{BeforeAndAfter, FunSuite}

/**
  * Class extended by all tests of sample. Sets up a dummy type system.
  *
  * @author Lucas Brutschy
  *
  */
trait SampleTest extends FunSuite with BeforeAndAfter {

  before {
    SystemParameters.tm = DummyTypeMap
  }

}
