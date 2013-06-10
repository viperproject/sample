package ch.ethz.inf.pm.sample.test

import org.scalatest.FlatSpec

/**
 * Run this before you do a check-in
 */
class TouchDevelopCheckInTest extends FlatSpec {

  "TouchBoost" should "have the same results for all scripts with aa" in {
     Run.main(List("-v","-p","Test/test/TouchDevelop/aa").toArray)
  }

}
