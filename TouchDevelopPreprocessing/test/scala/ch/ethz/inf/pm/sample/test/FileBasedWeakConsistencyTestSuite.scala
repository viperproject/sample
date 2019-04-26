/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.test

/**
  * Created by lucas on 20.10.16.
  */
class FileBasedWeakConsistencyTestSuite extends FileBasedTestSuite {

  override def testDirectories: Seq[String] = Seq("weakconsistency")

}
